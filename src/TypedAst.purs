module TypedAst where

import Ast as Ast
import Const as Const
import Data.List (List)
import Data.Maybe (Maybe)
import Types as Types

-- Typed expression: wraps InnerExp with a CType
type Exp = { e :: InnerExp, t :: Types.CType }

data InnerExp
  = Constant Const.ConstValue
  | Var String
  | EString String
  | Cast Types.CType Exp
  | Unary Ast.UnaryOperator Exp
  | Binary Ast.BinaryOperator Exp Exp
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

data Initializer
  = SingleInit Exp
  | CompoundInit Types.CType (List Initializer)

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
  , storageClass :: Maybe Ast.StorageClass
  }

data ForInit
  = InitDecl VariableDeclaration
  | InitExp (Maybe Exp)

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

data BlockItem
  = Stmt Statement
  | Decl Declaration

data Block = Block (List BlockItem)

type FunctionDeclaration =
  { name :: String
  , funType :: Types.CType
  , paramList :: List String
  , body :: Maybe Block
  , storageClass :: Maybe Ast.StorageClass
  }

data Declaration
  = FunDecl FunctionDeclaration
  | VarDecl VariableDeclaration
  | StructDecl StructDeclaration

data TypedProgram = Program (List Declaration)
