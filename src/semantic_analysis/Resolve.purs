module Resolve where

import Prelude

import Ast (Block(..), BlockItem(..), Declaration(..), Exp(..), ForInit(..), Initializer(..), Statement(..), StorageClass(..), UntypedProgram(..))
import CompilerError (CompilerError(..))
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import ResultUtil (resultFold, resultTraverse)
import Types (CType(..))
import UniqueIds as UniqueIds

type ResolvedVar =
  { unique_name :: String
  , from_current_scope :: Boolean
  , has_linkage :: Boolean
  }

type ResolvedStruct =
  { unique_tag :: String
  , struct_from_current_scope :: Boolean
  }

type IdMap = Map.Map String ResolvedVar
type StructMap = Map.Map String ResolvedStruct

-- F#'s Map.map takes (key -> value -> value)
copyIdentifierMap :: IdMap -> IdMap
copyIdentifierMap m =
  map (\entry -> entry { from_current_scope = false }) m

copyStructMap :: StructMap -> StructMap
copyStructMap m =
  map (\entry -> entry { struct_from_current_scope = false }) m

-- replace structure tags in type specifiers
resolveType :: StructMap -> CType -> Either String CType
resolveType struct_map = case _ of
  Structure tag ->
    case Map.lookup tag struct_map of
      Just entry -> Right (Structure entry.unique_tag)
      Nothing -> Left "specified undeclared structure type"
  Pointer referenced_t ->
    map Pointer (resolveType struct_map referenced_t)
  Array elem_type size ->
    map (\t -> Array t size) (resolveType struct_map elem_type)
  FunType param_types ret_type -> do
    resolved_param_types <- resultTraverse (resolveType struct_map) param_types
    resolved_ret_type <- resolveType struct_map ret_type
    Right (FunType resolved_param_types resolved_ret_type)
  t -> Right t

resolveExp :: StructMap -> IdMap -> Exp -> Either String Exp
resolveExp struct_map id_map = case _ of
  Assignment left right -> do
    l <- resolveExp struct_map id_map left
    r <- resolveExp struct_map id_map right
    Right (Assignment l r)
  Var v ->
    case Map.lookup v id_map of
      Just entry -> Right (Var entry.unique_name)
      Nothing -> Left ("Undeclared variable " <> v)
  Cast target_type e -> do
    resolved_type <- resolveType struct_map target_type
    resolved_e <- resolveExp struct_map id_map e
    Right (Cast resolved_type resolved_e)
  Unary op e ->
    map (\e' -> Unary op e') (resolveExp struct_map id_map e)
  Binary op e1 e2 -> do
    e1' <- resolveExp struct_map id_map e1
    e2' <- resolveExp struct_map id_map e2
    Right (Binary op e1' e2')
  Conditional condition then_result else_result -> do
    c <- resolveExp struct_map id_map condition
    t <- resolveExp struct_map id_map then_result
    e <- resolveExp struct_map id_map else_result
    Right (Conditional c t e)
  FunCall f args ->
    case Map.lookup f id_map of
      Just entry ->
        map (\resolved_args -> FunCall entry.unique_name resolved_args)
          (resultTraverse (resolveExp struct_map id_map) args)
      Nothing -> Left "Undeclared function!"
  Dereference inner ->
    map Dereference (resolveExp struct_map id_map inner)
  AddrOf inner ->
    map AddrOf (resolveExp struct_map id_map inner)
  Subscript ptr index -> do
    p <- resolveExp struct_map id_map ptr
    i <- resolveExp struct_map id_map index
    Right (Subscript p i)
  SizeOf e ->
    map SizeOf (resolveExp struct_map id_map e)
  SizeOfT t ->
    map SizeOfT (resolveType struct_map t)
  Dot strct mbr ->
    map (\s -> Dot s mbr) (resolveExp struct_map id_map strct)
  Arrow strct mbr ->
    map (\s -> Arrow s mbr) (resolveExp struct_map id_map strct)
  c@(Constant _) -> Right c
  c@(EString _) -> Right c

resolveOptionalExp :: StructMap -> IdMap -> Maybe Exp -> Either String (Maybe Exp)
resolveOptionalExp struct_map id_map = case _ of
  Just e -> map Just (resolveExp struct_map id_map e)
  Nothing -> Right Nothing

resolveLocalVarHelper :: UniqueIds.Counter -> IdMap -> String -> Maybe StorageClass -> Either String (Tuple UniqueIds.Counter (Tuple IdMap String))
resolveLocalVarHelper counter id_map name storage_class = do
  case Map.lookup name id_map of
    Just { from_current_scope: true, has_linkage } ->
      if not (has_linkage && storage_class == Just Extern) then
        Left "Duplicate variable declaration"
      else Right unit
    _ -> Right unit
  let result =
        if storage_class == Just Extern then
          Tuple counter { unique_name: name, from_current_scope: true, has_linkage: true }
        else
          let (Tuple counter' unique_name) = UniqueIds.makeNamedTemporary name counter
          in Tuple counter' { unique_name, from_current_scope: true, has_linkage: false }
  let (Tuple counter' entry) = result
  let new_map = Map.insert name entry id_map
  Right (Tuple counter' (Tuple new_map entry.unique_name))

resolveInitializer :: StructMap -> IdMap -> Initializer -> Either String Initializer
resolveInitializer struct_map id_map = case _ of
  SingleInit e ->
    map SingleInit (resolveExp struct_map id_map e)
  CompoundInit inits ->
    map CompoundInit (resultTraverse (resolveInitializer struct_map id_map) inits)

resolveLocalVarDeclaration :: UniqueIds.Counter -> StructMap -> IdMap
  -> { name :: String, varType :: CType, init :: Maybe Initializer, storageClass :: Maybe StorageClass }
  -> Either String (Tuple UniqueIds.Counter (Tuple IdMap { name :: String, varType :: CType, init :: Maybe Initializer, storageClass :: Maybe StorageClass }))
resolveLocalVarDeclaration counter struct_map id_map { name, varType: var_type, init, storageClass: storage_class } = do
  Tuple counter' (Tuple new_id_map unique_name) <-
    resolveLocalVarHelper counter id_map name storage_class
  resolved_type <- resolveType struct_map var_type
  resolved_init <-
    case init of
      Just i -> map Just (resolveInitializer struct_map new_id_map i)
      Nothing -> Right Nothing
  Right (Tuple counter' (Tuple new_id_map
    { name: unique_name
    , varType: resolved_type
    , init: resolved_init
    , storageClass: storage_class
    }))

resolveForInit :: UniqueIds.Counter -> StructMap -> IdMap -> ForInit -> Either String (Tuple UniqueIds.Counter (Tuple IdMap ForInit))
resolveForInit counter struct_map id_map = case _ of
  InitExp e ->
    map (\e' -> Tuple counter (Tuple id_map (InitExp e')))
      (resolveOptionalExp struct_map id_map e)
  InitDecl d ->
    map (\(Tuple counter' (Tuple new_id_map resolved_decl)) ->
        Tuple counter' (Tuple new_id_map (InitDecl resolved_decl)))
      (resolveLocalVarDeclaration counter struct_map id_map d)

resolveStatement :: UniqueIds.Counter -> StructMap -> IdMap -> Statement -> Either String (Tuple UniqueIds.Counter Statement)
resolveStatement counter struct_map id_map = case _ of
  Return e ->
    map (\resolved_e -> Tuple counter (Return resolved_e))
      (resolveOptionalExp struct_map id_map e)
  Expression e ->
    map (\e' -> Tuple counter (Expression e'))
      (resolveExp struct_map id_map e)
  If condition then_clause else_clause -> do
    Tuple counter' then' <- resolveStatement counter struct_map id_map then_clause
    Tuple counter'' else' <-
      case else_clause of
        Just e ->
          map (\(Tuple c e') -> Tuple c (Just e'))
            (resolveStatement counter' struct_map id_map e)
        Nothing -> Right (Tuple counter' Nothing)
    resolved_condition <- resolveExp struct_map id_map condition
    Right (Tuple counter'' (If resolved_condition then' else'))
  While condition body id -> do
    Tuple counter' body' <- resolveStatement counter struct_map id_map body
    resolved_condition <- resolveExp struct_map id_map condition
    Right (Tuple counter' (While resolved_condition body' id))
  DoWhile body condition id -> do
    Tuple counter' body' <- resolveStatement counter struct_map id_map body
    resolved_condition <- resolveExp struct_map id_map condition
    Right (Tuple counter' (DoWhile body' resolved_condition id))
  For init condition post body id -> do
    let id_map1 = copyIdentifierMap id_map
        struct_map1 = copyStructMap struct_map
    Tuple counter' (Tuple id_map2 resolved_init) <- resolveForInit counter struct_map1 id_map1 init
    Tuple counter'' body' <- resolveStatement counter' struct_map1 id_map2 body
    resolved_condition <- resolveOptionalExp struct_map1 id_map2 condition
    resolved_post <- resolveOptionalExp struct_map1 id_map2 post
    Right (Tuple counter'' (For resolved_init resolved_condition resolved_post body' id))
  Compound block -> do
    let new_variable_map = copyIdentifierMap id_map
        new_struct_map = copyStructMap struct_map
    Tuple counter' block' <- resolveBlock counter new_struct_map new_variable_map block
    Right (Tuple counter' (Compound block'))
  s@Null -> Right (Tuple counter s)
  s@(Break _) -> Right (Tuple counter s)
  s@(Continue _) -> Right (Tuple counter s)

resolveBlockItem :: UniqueIds.Counter -> Tuple StructMap IdMap -> BlockItem -> Either String (Tuple UniqueIds.Counter (Tuple (Tuple StructMap IdMap) BlockItem))
resolveBlockItem counter (Tuple struct_map id_map) = case _ of
  Stmt s ->
    map (\(Tuple counter' resolved_s) ->
        Tuple counter' (Tuple (Tuple struct_map id_map) (Stmt resolved_s)))
      (resolveStatement counter struct_map id_map s)
  Decl d ->
    map (\(Tuple counter' (Tuple new_maps resolved_d)) ->
        Tuple counter' (Tuple new_maps (Decl resolved_d)))
      (resolveLocalDeclaration counter struct_map id_map d)

resolveBlock :: UniqueIds.Counter -> StructMap -> IdMap -> Block -> Either String (Tuple UniqueIds.Counter Block)
resolveBlock counter struct_map id_map (Block items) =
  map (\(Tuple counter' (Tuple _ resolved_items)) ->
      Tuple counter' (Block resolved_items))
    ( resultFold (\(Tuple c (Tuple maps acc)) item ->
          map (\(Tuple c' (Tuple maps' item')) -> Tuple c' (Tuple maps' (acc <> (item' : Nil))))
            (resolveBlockItem c maps item))
        (Tuple counter (Tuple (Tuple struct_map id_map) Nil)) items
    )

resolveLocalDeclaration :: UniqueIds.Counter -> StructMap -> IdMap -> Declaration -> Either String (Tuple UniqueIds.Counter (Tuple (Tuple StructMap IdMap) Declaration))
resolveLocalDeclaration counter struct_map id_map = case _ of
  VarDecl vd ->
    map (\(Tuple counter' (Tuple new_id_map resolved_vd)) ->
        Tuple counter' (Tuple (Tuple struct_map new_id_map) (VarDecl resolved_vd)))
      (resolveLocalVarDeclaration counter struct_map id_map vd)
  FunDecl fd | fd.body /= Nothing ->
    Left "nested function definitions are not allowed"
  FunDecl fd | fd.storageClass == Just Static ->
    Left "static keyword not allowed on local function declarations"
  FunDecl fd ->
    map (\(Tuple counter' (Tuple new_id_map resolved_fd)) ->
        Tuple counter' (Tuple (Tuple struct_map new_id_map) (FunDecl resolved_fd)))
      (resolveFunctionDeclaration counter struct_map id_map fd)
  StructDecl sd ->
    map (\(Tuple counter' (Tuple new_struct_map resolved_sd)) ->
        Tuple counter' (Tuple (Tuple new_struct_map id_map) (StructDecl resolved_sd)))
      (resolveStructureDeclaration counter struct_map sd)

resolveParams :: UniqueIds.Counter -> IdMap -> List String -> Either String (Tuple UniqueIds.Counter (Tuple IdMap (List String)))
resolveParams counter id_map param_names =
  resultFold (\(Tuple c (Tuple m acc)) param_name ->
      map (\(Tuple c' (Tuple m' unique)) -> Tuple c' (Tuple m' (acc <> (unique : Nil))))
        (resolveLocalVarHelper c m param_name Nothing))
    (Tuple counter (Tuple id_map Nil)) param_names

resolveFunctionDeclaration :: UniqueIds.Counter -> StructMap -> IdMap
  -> { name :: String, funType :: CType, paramList :: List String, body :: Maybe Block, storageClass :: Maybe StorageClass }
  -> Either String (Tuple UniqueIds.Counter (Tuple IdMap { name :: String, funType :: CType, paramList :: List String, body :: Maybe Block, storageClass :: Maybe StorageClass }))
resolveFunctionDeclaration counter struct_map id_map fn = do
  case Map.lookup fn.name id_map of
    Just { from_current_scope: true, has_linkage: false } ->
      Left "Duplicate declaration"
    _ -> Right unit
  resolved_type <- resolveType struct_map fn.funType
  let new_entry = { unique_name: fn.name, from_current_scope: true, has_linkage: true }
      new_id_map = Map.insert fn.name new_entry id_map
      inner_id_map = copyIdentifierMap new_id_map
  Tuple counter' (Tuple inner_id_map1 resolved_params) <-
    resolveParams counter inner_id_map fn.paramList
  let inner_struct_map = copyStructMap struct_map
  Tuple counter'' resolved_body <-
    case fn.body of
      Just body ->
        map (\(Tuple c b) -> Tuple c (Just b))
          (resolveBlock counter' inner_struct_map inner_id_map1 body)
      Nothing -> Right (Tuple counter' Nothing)
  Right (Tuple counter'' (Tuple new_id_map
    { name: fn.name
    , funType: resolved_type
    , paramList: resolved_params
    , body: resolved_body
    , storageClass: fn.storageClass
    }))

resolveStructureDeclaration :: UniqueIds.Counter -> StructMap
  -> { tag :: String, members :: List { memberName :: String, memberType :: CType } }
  -> Either String (Tuple UniqueIds.Counter (Tuple StructMap { tag :: String, members :: List { memberName :: String, memberType :: CType } }))
resolveStructureDeclaration counter struct_map { tag, members } =
  let result =
        case Map.lookup tag struct_map of
          Just { unique_tag, struct_from_current_scope: true } ->
            Tuple counter (Tuple struct_map unique_tag)
          _ ->
            let (Tuple counter' unique_tag) = UniqueIds.makeNamedTemporary tag counter
                entry = { unique_tag, struct_from_current_scope: true }
            in Tuple counter' (Tuple (Map.insert tag entry struct_map) unique_tag)
      (Tuple counter' (Tuple new_map resolved_tag)) = result
      resolveMember m =
        map (\t -> m { memberType = t })
          (resolveType new_map m.memberType)
  in map (\resolved_members ->
        Tuple counter' (Tuple new_map { tag: resolved_tag, members: resolved_members }))
      (resultTraverse resolveMember members)

resolveFileScopeVariableDeclaration :: StructMap -> IdMap
  -> { name :: String, varType :: CType, init :: Maybe Initializer, storageClass :: Maybe StorageClass }
  -> Either String (Tuple IdMap { name :: String, varType :: CType, init :: Maybe Initializer, storageClass :: Maybe StorageClass })
resolveFileScopeVariableDeclaration struct_map id_map vd =
  map (\resolved_type ->
    let resolved_vd = vd { varType = resolved_type }
        new_map = Map.insert vd.name
          { unique_name: vd.name, from_current_scope: true, has_linkage: true }
          id_map
    in Tuple new_map resolved_vd)
    (resolveType struct_map vd.varType)

resolveGlobalDeclaration :: UniqueIds.Counter -> Tuple StructMap IdMap -> Declaration -> Either String (Tuple UniqueIds.Counter (Tuple (Tuple StructMap IdMap) Declaration))
resolveGlobalDeclaration counter (Tuple struct_map id_map) = case _ of
  FunDecl fd ->
    map (\(Tuple counter' (Tuple id_map1 fd')) ->
        Tuple counter' (Tuple (Tuple struct_map id_map1) (FunDecl fd')))
      (resolveFunctionDeclaration counter struct_map id_map fd)
  VarDecl vd ->
    map (\(Tuple id_map1 resolved_vd) ->
        Tuple counter (Tuple (Tuple struct_map id_map1) (VarDecl resolved_vd)))
      (resolveFileScopeVariableDeclaration struct_map id_map vd)
  StructDecl sd ->
    map (\(Tuple counter' (Tuple struct_map1 resolved_sd)) ->
        Tuple counter' (Tuple (Tuple struct_map1 id_map) (StructDecl resolved_sd)))
      (resolveStructureDeclaration counter struct_map sd)

resolve :: UniqueIds.Counter -> UntypedProgram -> Either String (Tuple UniqueIds.Counter UntypedProgram)
resolve counter (Program decls) =
  map (\(Tuple counter' (Tuple _ resolved_decls)) ->
      Tuple counter' (Program resolved_decls))
    ( resultFold (\(Tuple c (Tuple maps acc)) d ->
          map (\(Tuple c' (Tuple maps' d')) -> Tuple c' (Tuple maps' (acc <> (d' : Nil))))
            (resolveGlobalDeclaration c maps d))
        (Tuple counter (Tuple (Tuple (Map.empty :: StructMap) (Map.empty :: IdMap)) Nil)) decls
    )
