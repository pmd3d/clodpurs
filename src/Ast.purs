module Ast where

import Prelude

import Const as Const
import Data.List (List)
import Data.Maybe (Maybe)
import Types as Types

-- Operators (from Ast.Ops)

data UnaryOperator
  = Complement
  | Negate
  | Not

derive instance eqUnaryOperator :: Eq UnaryOperator
derive instance ordUnaryOperator :: Ord UnaryOperator
instance showUnaryOperator :: Show UnaryOperator where
  show Complement = "Complement"
  show Negate = "Negate"
  show Not = "Not"

data BinaryOperator
  = Add
  | Subtract
  | Multiply
  | Divide
  | Mod
  | And
  | Or
  | Equal
  | NotEqual
  | LessThan
  | LessOrEqual
  | GreaterThan
  | GreaterOrEqual

derive instance eqBinaryOperator :: Eq BinaryOperator
derive instance ordBinaryOperator :: Ord BinaryOperator
instance showBinaryOperator :: Show BinaryOperator where
  show Add = "Add"
  show Subtract = "Subtract"
  show Multiply = "Multiply"
  show Divide = "Divide"
  show Mod = "Mod"
  show And = "And"
  show Or = "Or"
  show Equal = "Equal"
  show NotEqual = "NotEqual"
  show LessThan = "LessThan"
  show LessOrEqual = "LessOrEqual"
  show GreaterThan = "GreaterThan"
  show GreaterOrEqual = "GreaterOrEqual"

-- Storage class (from Ast.StorageClass)

data StorageClass
  = Static
  | Extern

derive instance eqStorageClass :: Eq StorageClass
derive instance ordStorageClass :: Ord StorageClass
instance showStorageClass :: Show StorageClass where
  show Static = "Static"
  show Extern = "Extern"

-- Expressions (from Ast.UntypedExp) - Exp.String renamed to EString

data Exp
  = Constant Const.ConstValue
  | Var String
  | EString String
  | Cast Types.CType Exp
  | Unary UnaryOperator Exp
  | Binary BinaryOperator Exp Exp
  | Assignment Exp Exp
  | Conditional Exp Exp Exp
  | FunCall String (List Exp)
  | Dereference Exp
  | AddrOf Exp
  | Subscript Exp Exp
  | SizeOf Exp
  | SizeOfT Types.CType
  | Dot Exp String
  | Arrow Exp String

derive instance eqExp :: Eq Exp
derive instance ordExp :: Ord Exp

instance showExp :: Show Exp where
  show (Constant c) = "(Constant " <> show c <> ")"
  show (Var s) = "(Var " <> show s <> ")"
  show (EString s) = "(EString " <> show s <> ")"
  show (Cast t e) = "(Cast " <> show t <> " " <> show e <> ")"
  show (Unary op e) = "(Unary " <> show op <> " " <> show e <> ")"
  show (Binary op l r) = "(Binary " <> show op <> " " <> show l <> " " <> show r <> ")"
  show (Assignment l r) = "(Assignment " <> show l <> " " <> show r <> ")"
  show (Conditional c t e) = "(Conditional " <> show c <> " " <> show t <> " " <> show e <> ")"
  show (FunCall f args) = "(FunCall " <> show f <> " " <> show args <> ")"
  show (Dereference e) = "(Dereference " <> show e <> ")"
  show (AddrOf e) = "(AddrOf " <> show e <> ")"
  show (Subscript p i) = "(Subscript " <> show p <> " " <> show i <> ")"
  show (SizeOf e) = "(SizeOf " <> show e <> ")"
  show (SizeOfT t) = "(SizeOfT " <> show t <> ")"
  show (Dot s m) = "(Dot " <> show s <> " " <> show m <> ")"
  show (Arrow s m) = "(Arrow " <> show s <> " " <> show m <> ")"

data Initializer
  = SingleInit Exp
  | CompoundInit (List Initializer)

derive instance eqInitializer :: Eq Initializer
derive instance ordInitializer :: Ord Initializer

instance showInitializer :: Show Initializer where
  show (SingleInit e) = "(SingleInit " <> show e <> ")"
  show (CompoundInit l) = "(CompoundInit " <> show l <> ")"

-- Untyped AST (from Ast.Untyped)

type MemberDeclaration =
  { memberName :: String
  , memberType :: Types.CType
  }

type StructDeclaration =
  { tag :: String
  , members :: List MemberDeclaration
  }

type VariableDeclaration =
  { name :: String
  , varType :: Types.CType
  , init :: Maybe Initializer
  , storageClass :: Maybe StorageClass
  }

data ForInit
  = InitDecl VariableDeclaration
  | InitExp (Maybe Exp)

derive instance eqForInit :: Eq ForInit
instance showForInit :: Show ForInit where
  show (InitDecl vd) = "(InitDecl " <> show vd <> ")"
  show (InitExp e) = "(InitExp " <> show e <> ")"

data Statement
  = Return (Maybe Exp)
  | Expression Exp
  | If Exp Statement (Maybe Statement)
  | Compound Block
  | Break String
  | Continue String
  | While Exp Statement String
  | DoWhile Statement Exp String
  | For ForInit (Maybe Exp) (Maybe Exp) Statement String
  | Null

derive instance eqStatement :: Eq Statement
instance showStatement :: Show Statement where
  show (Return e) = "(Return " <> show e <> ")"
  show (Expression e) = "(Expression " <> show e <> ")"
  show (If c t e) = "(If " <> show c <> " " <> show t <> " " <> show e <> ")"
  show (Compound b) = "(Compound " <> show b <> ")"
  show (Break s) = "(Break " <> show s <> ")"
  show (Continue s) = "(Continue " <> show s <> ")"
  show (While c b i) = "(While " <> show c <> " " <> show b <> " " <> show i <> ")"
  show (DoWhile b c i) = "(DoWhile " <> show b <> " " <> show c <> " " <> show i <> ")"
  show (For i c p b id) = "(For " <> show i <> " " <> show c <> " " <> show p <> " " <> show b <> " " <> show id <> ")"
  show Null = "Null"

data BlockItem
  = Stmt Statement
  | Decl Declaration

derive instance eqBlockItem :: Eq BlockItem
instance showBlockItem :: Show BlockItem where
  show (Stmt s) = "(Stmt " <> show s <> ")"
  show (Decl d) = "(Decl " <> show d <> ")"

data Block = Block (List BlockItem)

derive instance eqBlock :: Eq Block
instance showBlock :: Show Block where
  show (Block items) = "(Block " <> show items <> ")"

type FunctionDeclaration =
  { name :: String
  , funType :: Types.CType
  , paramList :: List String
  , body :: Maybe Block
  , storageClass :: Maybe StorageClass
  }

data Declaration
  = FunDecl FunctionDeclaration
  | VarDecl VariableDeclaration
  | StructDecl StructDeclaration

derive instance eqDeclaration :: Eq Declaration
instance showDeclaration :: Show Declaration where
  show (FunDecl fd) = "(FunDecl " <> show fd <> ")"
  show (VarDecl vd) = "(VarDecl " <> show vd <> ")"
  show (StructDecl sd) = "(StructDecl " <> show sd <> ")"

data UntypedProgram = Program (List Declaration)

derive instance eqUntypedProgram :: Eq UntypedProgram
instance showUntypedProgram :: Show UntypedProgram where
  show (Program decls) = "(Program " <> show decls <> ")"
