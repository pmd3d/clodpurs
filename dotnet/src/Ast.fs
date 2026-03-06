module Ast

(* Unary and binary operators; used in exp AST nodes both with and without
   type information *)
module Ops =
    type UnaryOperator =
        | Complement
        | Negate
        | Not

    type BinaryOperator =
        | Add
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

(* Exp and initializer AST definitions without type info *)
module UntypedExp =
    open Ops

    type Exp =
        | Constant of Const.ConstValue
        | Var of string
        | String of string
        | Cast of targetType: Types.CType * e: Exp
        | Unary of UnaryOperator * Exp
        | Binary of BinaryOperator * Exp * Exp
        | Assignment of Exp * Exp
        | Conditional of condition: Exp * thenResult: Exp * elseResult: Exp
        | FunCall of f: string * args: Exp list
        | Dereference of Exp
        | AddrOf of Exp
        | Subscript of ptr: Exp * index: Exp
        | SizeOf of Exp
        | SizeOfT of Types.CType
        | Dot of strct: Exp * memberName: string
        | Arrow of strct: Exp * memberName: string

    type Initializer =
        | SingleInit of Exp
        | CompoundInit of Initializer list

(* Exp and initializer AST definitions with type info *)
module TypedExp =
    open Ops

    type InnerExp =
        | Constant of Const.ConstValue
        | Var of string
        | String of string
        | Cast of targetType: Types.CType * e: Exp
        | Unary of UnaryOperator * Exp
        | Binary of BinaryOperator * Exp * Exp
        | Assignment of Exp * Exp
        | Conditional of condition: Exp * thenResult: Exp * elseResult: Exp
        | FunCall of f: string * args: Exp list
        | Dereference of Exp
        | AddrOf of Exp
        | Subscript of ptr: Exp * index: Exp
        | SizeOf of Exp
        | SizeOfT of Types.CType
        | Dot of strct: Exp * memberName: string
        | Arrow of strct: Exp * memberName: string

    and Exp = { e: InnerExp; t: Types.CType }

    type Initializer =
        | SingleInit of Exp
        | CompoundInit of Types.CType * Initializer list

module StorageClass =
    type Kind =
        | Static
        | Extern

(* The complete untyped AST *)
module Untyped =
    open Ops
    open StorageClass

    type UnaryOperator = Ops.UnaryOperator
    type BinaryOperator = Ops.BinaryOperator
    type StorageClass = StorageClass.Kind

    type Exp = UntypedExp.Exp
    type Initializer = UntypedExp.Initializer

    type MemberDeclaration =
        { memberName: string
          memberType: Types.CType }

    type StructDeclaration =
        { tag: string
          members: MemberDeclaration list }

    type VariableDeclaration =
        { name: string
          varType: Types.CType
          init: Initializer option
          storageClass: StorageClass option }

    type ForInit =
        | InitDecl of VariableDeclaration
        | InitExp of Exp option

    type Statement =
        | Return of Exp option
        | Expression of Exp
        | If of condition: Exp * thenClause: Statement * elseClause: Statement option
        | Compound of Block
        | Break of string
        | Continue of string
        | While of condition: Exp * body: Statement * id: string
        | DoWhile of body: Statement * condition: Exp * id: string
        | For of
            init: ForInit *
            condition: Exp option *
            post: Exp option *
            body: Statement *
            id: string
        | Null

    and BlockItem =
        | Stmt of Statement
        | Decl of Declaration

    and Block = Block of BlockItem list

    and FunctionDeclaration =
        { name: string
          funType: Types.CType
          paramList: string list
          body: Block option
          storageClass: StorageClass option }

    and Declaration =
        | FunDecl of FunctionDeclaration
        | VarDecl of VariableDeclaration
        | StructDecl of StructDeclaration

    type UntypedProgram = Program of Declaration list

(* The complete typed AST *)
module Typed =
    open Ops
    open StorageClass

    type UnaryOperator = Ops.UnaryOperator
    type BinaryOperator = Ops.BinaryOperator
    type StorageClass = StorageClass.Kind

    type InnerExp = TypedExp.InnerExp
    type Exp = TypedExp.Exp
    type Initializer = TypedExp.Initializer

    type MemberDeclaration =
        { memberName: string
          memberType: Types.CType }

    type StructDeclaration =
        { tag: string
          members: MemberDeclaration list }

    type VariableDeclaration =
        { name: string
          varType: Types.CType
          init: Initializer option
          storageClass: StorageClass option }

    type ForInit =
        | InitDecl of VariableDeclaration
        | InitExp of Exp option

    type Statement =
        | Return of Exp option
        | Expression of Exp
        | If of condition: Exp * thenClause: Statement * elseClause: Statement option
        | Compound of Block
        | Break of string
        | Continue of string
        | While of condition: Exp * body: Statement * id: string
        | DoWhile of body: Statement * condition: Exp * id: string
        | For of
            init: ForInit *
            condition: Exp option *
            post: Exp option *
            body: Statement *
            id: string
        | Null

    and BlockItem =
        | Stmt of Statement
        | Decl of Declaration

    and Block = Block of BlockItem list

    and FunctionDeclaration =
        { name: string
          funType: Types.CType
          paramList: string list
          body: Block option
          storageClass: StorageClass option }

    and Declaration =
        | FunDecl of FunctionDeclaration
        | VarDecl of VariableDeclaration
        | StructDecl of StructDeclaration

    type TypedProgram = Program of Declaration list
