module LabelLoops where

import Prelude

import Ast (Block(..), BlockItem(..), Declaration(..), Statement(..), UntypedProgram(..))
import CompilerError (CompilerError(..))
import Data.Either (Either(..))
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import ResultUtil (resultFold)
import UniqueIds as UniqueIds

labelStatement :: UniqueIds.Counter -> Maybe String -> Statement -> Either CompilerError (Tuple UniqueIds.Counter Statement)
labelStatement counter currentLabel = case _ of
  Break _ ->
    case currentLabel of
      Just l -> Right (Tuple counter (Break l))
      Nothing -> Left (LoopLabelError "Break outside of loop")
  Continue _ ->
    case currentLabel of
      Just l -> Right (Tuple counter (Continue l))
      Nothing -> Left (LoopLabelError "Continue outside of loop")
  While condition body _id ->
    let (Tuple counter' newId) = UniqueIds.makeLabel "while" counter
    in map (\(Tuple counter'' body') ->
        Tuple counter'' (While condition body' newId))
      (labelStatement counter' (Just newId) body)
  DoWhile body condition _id ->
    let (Tuple counter' newId) = UniqueIds.makeLabel "do_while" counter
    in map (\(Tuple counter'' body') ->
        Tuple counter'' (DoWhile body' condition newId))
      (labelStatement counter' (Just newId) body)
  For init condition post body _id ->
    let (Tuple counter' newId) = UniqueIds.makeLabel "for" counter
    in map (\(Tuple counter'' body') ->
        Tuple counter'' (For init condition post body' newId))
      (labelStatement counter' (Just newId) body)
  Compound blk ->
    map (\(Tuple counter' blk') -> Tuple counter' (Compound blk'))
      (labelBlock counter currentLabel blk)
  If condition thenClause elseClause -> do
    Tuple counter' thenClause' <- labelStatement counter currentLabel thenClause
    Tuple counter'' elseClause' <- case elseClause of
      Just e ->
        map (\(Tuple c e') -> Tuple c (Just e'))
          (labelStatement counter' currentLabel e)
      Nothing -> Right (Tuple counter' Nothing)
    Right (Tuple counter'' (If condition thenClause' elseClause'))
  s@Null -> Right (Tuple counter s)
  s@(Return _) -> Right (Tuple counter s)
  s@(Expression _) -> Right (Tuple counter s)

labelBlockItem :: UniqueIds.Counter -> Maybe String -> BlockItem -> Either CompilerError (Tuple UniqueIds.Counter BlockItem)
labelBlockItem counter currentLabel = case _ of
  Stmt s ->
    map (\(Tuple counter' s') -> Tuple counter' (Stmt s'))
      (labelStatement counter currentLabel s)
  decl -> Right (Tuple counter decl)

labelBlock :: UniqueIds.Counter -> Maybe String -> Block -> Either CompilerError (Tuple UniqueIds.Counter Block)
labelBlock counter currentLabel (Block b) =
  map (\(Tuple counter' items) -> Tuple counter' (Block items))
    ( resultFold
        ( \(Tuple c acc) item ->
            map (\(Tuple c' item') -> Tuple c' (acc <> Cons item' Nil))
              (labelBlockItem c currentLabel item)
        )
        (Tuple counter Nil)
        b
    )

labelDecl :: UniqueIds.Counter -> Declaration -> Either CompilerError (Tuple UniqueIds.Counter Declaration)
labelDecl counter = case _ of
  FunDecl fn ->
    case fn.body of
      Just body ->
        map (\(Tuple counter' body') ->
          Tuple counter' (FunDecl (fn { body = Just body' })))
          (labelBlock counter Nothing body)
      Nothing -> Right (Tuple counter (FunDecl fn))
  varDecl -> Right (Tuple counter varDecl)

labelLoops :: UniqueIds.Counter -> UntypedProgram -> Either CompilerError (Tuple UniqueIds.Counter UntypedProgram)
labelLoops counter (Program decls) =
  map (\(Tuple counter' decls') -> Tuple counter' (Program decls'))
    ( resultFold
        ( \(Tuple c acc) d ->
            map (\(Tuple c' d') -> Tuple c' (acc <> Cons d' Nil))
              (labelDecl c d)
        )
        (Tuple counter Nil)
        decls
    )
