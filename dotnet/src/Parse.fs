module Parse

open System.Numerics
open System
open ResultCE

module T = Tokens
module Ast = Ast.Untyped

// --- Peek helpers (Option-based, never fail) ---

let private peekOpt (tokens: TokStream.TokStream) = Stream.peek tokens

let private peekEq tok tokens = peekOpt tokens = Some tok

let private peekIs pred tokens =
    match peekOpt tokens with
    | Some t -> pred t
    | None -> false

// --- Error formatting ---

type private Expected =
    | Tok of T.Token
    | Name of string

let private ppExpected =
    function
    | Tok tk -> Tokens.show tk
    | Name s -> s

let private formatError expected actual =
    sprintf "Expected %s but found %s"
        (ppExpected expected)
        (Tokens.show actual)

// --- Core token operations (Result-returning) ---

let private takeToken (tokens: TokStream.TokStream) =
    TokStream.takeToken tokens

let private expect expected (tokens: TokStream.TokStream) =
    result {
        let! actual, tokens = takeToken tokens
        if actual <> expected then
            return! Error (formatError (Tok expected) actual)
        else
            return tokens
    }

// --- Pure helpers (no failure possible) ---

let private unescape (s: string) =
    let escapes =
        [ ("\\'", char 39)
          ("\\\"", char 34)
          ("\\?", char 63)
          ("\\\\", char 92)
          ("\\a", char 7)
          ("\\b", char 8)
          ("\\f", char 12)
          ("\\n", char 10)
          ("\\r", char 13)
          ("\\t", char 9)
          ("\\v", char 11) ]

    let rec unescapeNext (remaining: string) =
        if remaining = "" then
            []
        else
            let findMatchingEscape (escapeSeq: string, _) =
                remaining.StartsWith(escapeSeq)
            match List.tryFind findMatchingEscape escapes with
            | Some(escapeSeq, unescaped) ->
                unescaped
                :: unescapeNext
                       (StringUtil.drop escapeSeq.Length remaining)
            | None ->
                remaining.[0]
                :: unescapeNext (StringUtil.drop 1 remaining)

    let unescapedList = unescapeNext s
    StringUtil.ofList unescapedList

let private isIdent =
    function
    | T.Identifier _ -> true
    | _ -> false

let private isTypeSpecifier =
    function
    | T.KWInt
    | T.KWLong
    | T.KWUnsigned
    | T.KWSigned
    | T.KWDouble
    | T.KWChar
    | T.KWVoid
    | T.KWStruct -> true
    | _ -> false

let private isSpecifier =
    function
    | T.KWStatic | T.KWExtern -> true
    | other -> isTypeSpecifier other

let private getPrecedence =
    function
    | T.Star | T.Slash | T.Percent -> Some 50
    | T.Plus | T.Hyphen -> Some 45
    | T.LessThan | T.LessOrEqual | T.GreaterThan | T.GreaterOrEqual ->
        Some 35
    | T.DoubleEqual | T.NotEqual -> Some 30
    | T.LogicalAnd -> Some 10
    | T.LogicalOr -> Some 5
    | T.QuestionMark -> Some 3
    | T.EqualSign -> Some 1
    | _ -> None

// --- Pure functions that can fail (return Result) ---

let private parseStorageClass =
    function
    | T.KWExtern -> Ok Ast.StorageClass.Extern
    | T.KWStatic -> Ok Ast.StorageClass.Static
    | other -> Error (formatError (Name "a storage class specifier") other)

let private parseType specifierList =
    let specifierList = List.sortWith Tokens.compare specifierList
    match specifierList with
    | [ T.Identifier tag ] -> Ok (Types.Structure tag)
    | [ T.KWVoid ] -> Ok Types.Void
    | [ T.KWDouble ] -> Ok Types.Double
    | [ T.KWChar ] -> Ok Types.Char
    | [ T.KWChar; T.KWSigned ] -> Ok Types.SChar
    | [ T.KWChar; T.KWUnsigned ] -> Ok Types.UChar
    | _ ->
        if
            specifierList = []
            || List.sortWith compare (List.distinct specifierList)
               <> List.sortWith compare specifierList
            || List.contains T.KWDouble specifierList
            || List.contains T.KWChar specifierList
            || List.contains T.KWVoid specifierList
            || List.exists isIdent specifierList
            || List.contains T.KWSigned specifierList
               && List.contains T.KWUnsigned specifierList
        then
            Error "Invalid type specifier"
        else if
            List.contains T.KWUnsigned specifierList
            && List.contains T.KWLong specifierList
        then
            Ok Types.ULong
        else if List.contains T.KWUnsigned specifierList then
            Ok Types.UInt
        else if List.contains T.KWLong specifierList then
            Ok Types.Long
        else
            Ok Types.Int

let private parseSignedConstant token =
    result {
        let! v, isInt =
            match token with
            | T.ConstInt i -> Ok (i, true)
            | T.ConstLong l -> Ok (l, false)
            | other ->
                Error (formatError (Name "a signed integer constant") other)
        if v > (BigInteger.Pow(2I, 63) - 1I) then
            return!
                Error
                    "Constant is too large to represent as an int or long"
        else if isInt && v <= (BigInteger.Pow(2I, 31) - 1I) then
            return Const.ConstInt(int32 v)
        else
            return Const.ConstLong(int64 v)
    }

let private parseUnsignedConstant token =
    result {
        let! v, isUint =
            match token with
            | T.ConstUInt ui -> Ok (ui, true)
            | T.ConstULong ul -> Ok (ul, false)
            | other ->
                Error (formatError (Name "an unsigned integer  constant") other)
        if v > (BigInteger.Pow(2I, 64) - 1I) then
            return!
                Error
                    "Constant is too large to represent as an unsigned int or unsigned \
                     long"
        else if isUint && v <= (BigInteger.Pow(2I, 32) - 1I) then
            return Const.ConstUInt(uint32 v)
        else
            return Const.ConstULong(uint64 v)
    }

let private parseChar token =
    let unescaped = unescape token
    if String.length unescaped = 1 then
        let chCode = int unescaped.[0]
        Ok (Const.ConstInt(int32 chCode))
    else
        Error "multi-character constant tokens not supported"

let private parseTypeAndStorageClass specifierList =
    result {
        let types, storageClasses =
            List.partition
                (fun t -> isTypeSpecifier t || isIdent t)
                specifierList
        let! typ = parseType types
        let! storageClass =
            match storageClasses with
            | [] -> Ok None
            | [ sc ] ->
                result {
                    let! parsed = parseStorageClass sc
                    return Some parsed
                }
            | _ :: _ -> Error "Internal error - not a storage class"
        return (typ, storageClass)
    }

// --- Parsing functions (return Result<'a * TokStream, string>) ---

let private parseId (tokens: TokStream.TokStream) =
    result {
        let! tok, tokens = takeToken tokens
        match tok with
        | T.Identifier x -> return (x, tokens)
        | other -> return! Error (formatError (Name "an identifier") other)
    }

let private parseTypeSpecifier (tokens: TokStream.TokStream) =
    result {
        let! spec, tokens = takeToken tokens
        match spec with
        | T.KWStruct ->
            let! expectedTag, tokens = takeToken tokens
            if isIdent expectedTag then
                return (expectedTag, tokens)
            else
                return! Error (formatError (Name "a structure tag") expectedTag)
        | _ ->
            if isTypeSpecifier spec then
                return (spec, tokens)
            else
                return! Error (formatError (Name "a type specifier") spec)
    }

let rec private parseTypeSpecifierList (tokens: TokStream.TokStream) =
    result {
        let! spec, tokens = parseTypeSpecifier tokens
        if peekIs isTypeSpecifier tokens then
            let! rest, tokens = parseTypeSpecifierList tokens
            return (spec :: rest, tokens)
        else
            return ([ spec ], tokens)
    }

let private parseSpecifier (tokens: TokStream.TokStream) =
    result {
        match peekOpt tokens with
        | Some spec when isTypeSpecifier spec ->
            return! parseTypeSpecifier tokens
        | Some spec when isSpecifier spec ->
            let! spec, tokens = takeToken tokens
            return (spec, tokens)
        | Some spec ->
            return! Error (formatError (Name "a type or storage-class specifier") spec)
        | None ->
            return! Error "Unexpected end of file"
    }

let rec private parseSpecifierList (tokens: TokStream.TokStream) =
    result {
        let! spec, tokens = parseSpecifier tokens
        if peekIs isSpecifier tokens then
            let! rest, tokens = parseSpecifierList tokens
            return (spec :: rest, tokens)
        else
            return ([ spec ], tokens)
    }

let parseConst (tokens: TokStream.TokStream) =
    result {
        let! constTok, tokens = takeToken tokens
        let! constVal =
            match constTok with
            | T.ConstInt _ | T.ConstLong _ -> parseSignedConstant constTok
            | T.ConstUInt _ | T.ConstULong _ -> parseUnsignedConstant constTok
            | T.ConstDouble d -> Ok (Const.ConstDouble d)
            | T.ConstChar c -> parseChar c
            | other -> Error (formatError (Name "a constant token") other)
        return (constVal, tokens)
    }

let private parseDim (tokens: TokStream.TokStream) =
    result {
        let! tokens = expect T.OpenBracket tokens
        let! c, tokens = parseConst tokens
        let! dim =
            match c with
            | Const.ConstDouble _ ->
                Error "Floating-point array dimensions not allowed"
            | Const.ConstChar c -> Ok (int64 c)
            | Const.ConstInt i -> Ok (int64 i)
            | Const.ConstLong l -> Ok (int64 l)
            | Const.ConstUChar uc -> Ok (int64 uc)
            | Const.ConstUInt u -> Ok (int64 u)
            | Const.ConstULong ul -> Ok (int64 ul)
        let! tokens = expect T.CloseBracket tokens
        return (dim, tokens)
    }

let private parseString (tokens: TokStream.TokStream) =
    result {
        let! tok, tokens = takeToken tokens
        match tok with
        | T.StringLiteral s -> return (unescape s, tokens)
        | other -> return! Error (formatError (Name "a string literal") other)
    }

type private AbstractDeclarator =
    | AbstractPointer of AbstractDeclarator
    | AbstractArray of AbstractDeclarator * int64
    | AbstractBase

let rec private parseAbstractArrayDeclSuffix baseDecl (tokens: TokStream.TokStream) =
    result {
        let! dim, tokens = parseDim tokens
        let newDecl = AbstractArray(baseDecl, dim)
        if peekEq T.OpenBracket tokens then
            return! parseAbstractArrayDeclSuffix newDecl tokens
        else
            return (newDecl, tokens)
    }

let rec private parseAbstractDeclarator (tokens: TokStream.TokStream) =
    result {
        match peekOpt tokens with
        | Some T.Star ->
            let! _, tokens = takeToken tokens
            let! inner, tokens =
                match peekOpt tokens with
                | Some T.Star | Some T.OpenParen | Some T.OpenBracket ->
                    parseAbstractDeclarator tokens
                | _ -> Ok (AbstractBase, tokens)
            return (AbstractPointer inner, tokens)
        | _ -> return! parseDirectAbstractDeclarator tokens
    }

and private parseDirectAbstractDeclarator (tokens: TokStream.TokStream) =
    result {
        if peekEq T.OpenParen tokens then
            let! _, tokens = takeToken tokens
            let! decl, tokens = parseAbstractDeclarator tokens
            let! tokens = expect T.CloseParen tokens
            if peekEq T.OpenBracket tokens then
                return! parseAbstractArrayDeclSuffix decl tokens
            else
                return (decl, tokens)
        else
            return! parseAbstractArrayDeclSuffix AbstractBase tokens
    }

let rec private processAbstractDeclarator decl baseType =
    match decl with
    | AbstractBase -> baseType
    | AbstractPointer inner ->
        let derivedType = Types.Pointer baseType
        processAbstractDeclarator inner derivedType
    | AbstractArray(inner, size) ->
        let derivedType = Types.Array(baseType, size)
        processAbstractDeclarator inner derivedType

let private parseUnop (tokens: TokStream.TokStream) =
    result {
        let! tok, tokens = takeToken tokens
        let! op =
            match tok with
            | T.Tilde -> Ok Ast.UnaryOperator.Complement
            | T.Hyphen -> Ok Ast.UnaryOperator.Negate
            | T.Bang -> Ok Ast.UnaryOperator.Not
            | other -> Error (formatError (Name "a unary operator") other)
        return (op, tokens)
    }

let private parseBinop (tokens: TokStream.TokStream) =
    result {
        let! tok, tokens = takeToken tokens
        let! op =
            match tok with
            | T.Plus -> Ok Ast.BinaryOperator.Add
            | T.Hyphen -> Ok Ast.BinaryOperator.Subtract
            | T.Star -> Ok Ast.BinaryOperator.Multiply
            | T.Slash -> Ok Ast.BinaryOperator.Divide
            | T.Percent -> Ok Ast.BinaryOperator.Mod
            | T.LogicalAnd -> Ok Ast.BinaryOperator.And
            | T.LogicalOr -> Ok Ast.BinaryOperator.Or
            | T.DoubleEqual -> Ok Ast.BinaryOperator.Equal
            | T.NotEqual -> Ok Ast.BinaryOperator.NotEqual
            | T.LessThan -> Ok Ast.BinaryOperator.LessThan
            | T.LessOrEqual -> Ok Ast.BinaryOperator.LessOrEqual
            | T.GreaterThan -> Ok Ast.BinaryOperator.GreaterThan
            | T.GreaterOrEqual -> Ok Ast.BinaryOperator.GreaterOrEqual
            | other -> Error (formatError (Name "a binary operator") other)
        return (op, tokens)
    }

let private parseTypeName (tokens: TokStream.TokStream) =
    result {
        let! typeSpecifiers, tokens = parseTypeSpecifierList tokens
        let! baseType = parseType typeSpecifiers
        match peekOpt tokens with
        | Some T.CloseParen -> return (baseType, tokens)
        | _ ->
            let! abstractDecl, tokens = parseAbstractDeclarator tokens
            return (processAbstractDeclarator abstractDecl baseType, tokens)
    }

let rec private parsePrimaryExp (tokens: TokStream.TokStream) =
    result {
        match peekOpt tokens with
        | Some (T.ConstInt _)
        | Some (T.ConstLong _)
        | Some (T.ConstUInt _)
        | Some (T.ConstULong _)
        | Some (T.ConstDouble _)
        | Some (T.ConstChar _) ->
            let! c, tokens = parseConst tokens
            return (Ast.Exp.Constant c, tokens)
        | Some (T.Identifier _) ->
            let! id, tokens = parseId tokens
            if peekEq T.OpenParen tokens then
                let! _, tokens = takeToken tokens
                let! args, tokens =
                    if peekEq T.CloseParen tokens then
                        Ok ([], tokens)
                    else
                        parseArgumentList tokens
                let! tokens = expect T.CloseParen tokens
                return (Ast.Exp.FunCall(id, args), tokens)
            else
                return (Ast.Exp.Var id, tokens)
        | Some T.OpenParen ->
            let! _, tokens = takeToken tokens
            let! e, tokens = parseExp 0 tokens
            let! tokens = expect T.CloseParen tokens
            return (e, tokens)
        | Some (T.StringLiteral _) ->
            let rec parseStringLoop tokens =
                result {
                    let! s, tokens = parseString tokens
                    match peekOpt tokens with
                    | Some (T.StringLiteral _) ->
                        let! rest, tokens = parseStringLoop tokens
                        return (s + rest, tokens)
                    | _ -> return (s, tokens)
                }
            let! s, tokens = parseStringLoop tokens
            return (Ast.Exp.String s, tokens)
        | Some t -> return! Error (formatError (Name "a primary expression") t)
        | None -> return! Error "Unexpected end of file"
    }

and private parseArgumentList (tokens: TokStream.TokStream) =
    result {
        let! arg, tokens = parseExp 0 tokens
        if peekEq T.Comma tokens then
            let! _, tokens = takeToken tokens
            let! rest, tokens = parseArgumentList tokens
            return (arg :: rest, tokens)
        else
            return ([ arg ], tokens)
    }

and private parsePostfixExp (tokens: TokStream.TokStream) =
    result {
        let! primary, tokens = parsePrimaryExp tokens

        let rec postfixLoop e tokens =
            result {
                match peekOpt tokens with
                | Some T.OpenBracket ->
                    let! _, tokens = takeToken tokens
                    let! subscript, tokens = parseExp 0 tokens
                    let! tokens = expect T.CloseBracket tokens
                    let subscriptExp = Ast.Exp.Subscript(e, subscript)
                    return! postfixLoop subscriptExp tokens
                | Some T.Dot ->
                    let! _, tokens = takeToken tokens
                    let! memberName, tokens = parseId tokens
                    let dotExp = Ast.Exp.Dot(e, memberName)
                    return! postfixLoop dotExp tokens
                | Some T.Arrow ->
                    let! _, tokens = takeToken tokens
                    let! memberName, tokens = parseId tokens
                    let arrowExp = Ast.Exp.Arrow(e, memberName)
                    return! postfixLoop arrowExp tokens
                | _ -> return (e, tokens)
            }

        return! postfixLoop primary tokens
    }

and private parseUnaryExp (tokens: TokStream.TokStream) =
    result {
        match TokStream.npeek 3 tokens with
        | T.Star :: _ ->
            let! _, tokens = takeToken tokens
            let! innerExp, tokens = parseCastExp tokens
            return (Ast.Exp.Dereference innerExp, tokens)
        | T.Ampersand :: _ ->
            let! _, tokens = takeToken tokens
            let! innerExp, tokens = parseCastExp tokens
            return (Ast.Exp.AddrOf innerExp, tokens)
        | (T.Hyphen | T.Tilde | T.Bang) :: _ ->
            let! operator, tokens = parseUnop tokens
            let! innerExp, tokens = parseCastExp tokens
            return (Ast.Exp.Unary(operator, innerExp), tokens)
        | [ T.KWSizeOf; T.OpenParen; t ] when isTypeSpecifier t ->
            let! _, tokens = takeToken tokens
            let! _, tokens = takeToken tokens
            let! typ, tokens = parseTypeName tokens
            let! tokens = expect T.CloseParen tokens
            return (Ast.Exp.SizeOfT typ, tokens)
        | T.KWSizeOf :: _ ->
            let! _, tokens = takeToken tokens
            let! innerExp, tokens = parseUnaryExp tokens
            return (Ast.Exp.SizeOf innerExp, tokens)
        | _ -> return! parsePostfixExp tokens
    }

and private parseCastExp (tokens: TokStream.TokStream) =
    result {
        match TokStream.npeek 2 tokens with
        | [ T.OpenParen; t ] when isTypeSpecifier t ->
            let! _, tokens = takeToken tokens
            let! targetType, tokens = parseTypeName tokens
            let! tokens = expect T.CloseParen tokens
            let! innerExp, tokens = parseCastExp tokens
            return (Ast.Exp.Cast(targetType, innerExp), tokens)
        | _ -> return! parseUnaryExp tokens
    }

and private parseConditionalMiddle (tokens: TokStream.TokStream) =
    result {
        let! tokens = expect T.QuestionMark tokens
        let! e, tokens = parseExp 0 tokens
        let! tokens = expect T.Colon tokens
        return (e, tokens)
    }

and parseExp minPrec (tokens: TokStream.TokStream) =
    result {
        let! initialFactor, tokens = parseCastExp tokens

        let rec parseExpLoop left tokens =
            result {
                match peekOpt tokens with
                | Some next ->
                    match getPrecedence next with
                    | Some prec when prec >= minPrec ->
                        let! left, tokens =
                            if next = T.EqualSign then
                                result {
                                    let! _, tokens = takeToken tokens
                                    let! right, tokens = parseExp prec tokens
                                    return (Ast.Exp.Assignment(left, right), tokens)
                                }
                            else if next = T.QuestionMark then
                                result {
                                    let! middle, tokens = parseConditionalMiddle tokens
                                    let! right, tokens = parseExp prec tokens
                                    return (Ast.Exp.Conditional(left, middle, right), tokens)
                                }
                            else
                                result {
                                    let! operator, tokens = parseBinop tokens
                                    let! right, tokens = parseExp (prec + 1) tokens
                                    return (Ast.Exp.Binary(operator, left, right), tokens)
                                }
                        return! parseExpLoop left tokens
                    | _ -> return (left, tokens)
                | None -> return (left, tokens)
            }

        return! parseExpLoop initialFactor tokens
    }

let private parseOptionalExp delim (tokens: TokStream.TokStream) =
    result {
        if peekEq delim tokens then
            let! _, tokens = takeToken tokens
            return (None, tokens)
        else
            let! e, tokens = parseExp 0 tokens
            let! tokens = expect delim tokens
            return (Some e, tokens)
    }

type private Declarator =
    | Ident of string
    | PointerDeclarator of Declarator
    | ArrayDeclarator of Declarator * int64
    | FunDeclarator of ParamInfo list * Declarator

and private ParamInfo = Param of Types.CType * Declarator

let rec private parseArrayDeclSuffix baseDecl (tokens: TokStream.TokStream) =
    result {
        let! dim, tokens = parseDim tokens
        let newDecl = ArrayDeclarator(baseDecl, dim)
        if peekEq T.OpenBracket tokens then
            return! parseArrayDeclSuffix newDecl tokens
        else
            return (newDecl, tokens)
    }

let rec private parseDeclarator (tokens: TokStream.TokStream) =
    result {
        match peekOpt tokens with
        | Some T.Star ->
            let! _, tokens = takeToken tokens
            let! inner, tokens = parseDeclarator tokens
            return (PointerDeclarator inner, tokens)
        | _ -> return! parseDirectDeclarator tokens
    }

and private parseDirectDeclarator (tokens: TokStream.TokStream) =
    result {
        let! simpleDec, tokens = parseSimpleDeclarator tokens
        match peekOpt tokens with
        | Some T.OpenParen ->
            let! paramList, tokens = parseParamList tokens
            return (FunDeclarator(paramList, simpleDec), tokens)
        | Some T.OpenBracket -> return! parseArrayDeclSuffix simpleDec tokens
        | _ -> return (simpleDec, tokens)
    }

and private parseParamList (tokens: TokStream.TokStream) =
    result {
        if TokStream.npeek 2 tokens = [ T.OpenParen; T.CloseParen ] then
            let! _, tokens = takeToken tokens
            let! _, tokens = takeToken tokens
            return ([], tokens)
        else if TokStream.npeek 3 tokens = [ T.OpenParen; T.KWVoid; T.CloseParen ] then
            let! _, tokens = takeToken tokens
            let! _, tokens = takeToken tokens
            let! _, tokens = takeToken tokens
            return ([], tokens)
        else
            let! tokens = expect T.OpenParen tokens
            let rec paramLoop tokens =
                result {
                    let! nextParam, tokens = parseParam tokens
                    if peekEq T.Comma tokens then
                        let! _, tokens = takeToken tokens
                        let! rest, tokens = paramLoop tokens
                        return (nextParam :: rest, tokens)
                    else
                        return ([ nextParam ], tokens)
                }
            let! paramList, tokens = paramLoop tokens
            let! tokens = expect T.CloseParen tokens
            return (paramList, tokens)
    }

and private parseParam (tokens: TokStream.TokStream) =
    result {
        let! specs, tokens = parseTypeSpecifierList tokens
        let! paramT = parseType specs
        let! paramDecl, tokens = parseDeclarator tokens
        return (Param(paramT, paramDecl), tokens)
    }

and private parseSimpleDeclarator (tokens: TokStream.TokStream) =
    result {
        let! nextTok, tokens = takeToken tokens
        match nextTok with
        | T.OpenParen ->
            let! decl, tokens = parseDeclarator tokens
            let! tokens = expect T.CloseParen tokens
            return (decl, tokens)
        | T.Identifier id -> return (Ident id, tokens)
        | other -> return! Error (formatError (Name "a simple declarator") other)
    }

let rec private processDeclarator decl baseType =
    result {
        match decl with
        | Ident s -> return (s, baseType, [])
        | PointerDeclarator d ->
            let derivedType = Types.Pointer baseType
            return! processDeclarator d derivedType
        | ArrayDeclarator(inner, size) ->
            let derivedType = Types.Array(baseType, size)
            return! processDeclarator inner derivedType
        | FunDeclarator(paramList, Ident s) ->
            let processParam (Param(pBaseType, pDecl)) =
                result {
                    let! paramName, paramT, _ = processDeclarator pDecl pBaseType
                    match paramT with
                    | Types.FunType _ ->
                        return!
                            Error
                                "Function pointers in parameters are not supported"
                    | _ -> ()
                    return (paramName, paramT)
                }
            let! processedParams =
                paramList
                |> List.fold (fun acc p ->
                    result {
                        let! accList = acc
                        let! processed = processParam p
                        return accList @ [ processed ]
                    }) (Ok [])
            let paramNames, paramTypes = List.unzip processedParams
            let funType =
                Types.FunType(paramTypes, baseType)
            return (s, funType, paramNames)
        | FunDeclarator _ ->
            return!
                Error
                    "can't apply additional type derivations to a function declarator"
    }

let rec private parseInitializer (tokens: TokStream.TokStream) =
    result {
        if peekEq T.OpenBrace tokens then
            let! _, tokens = takeToken tokens
            let rec parseInitLoop tokens =
                result {
                    let! nextInit, tokens = parseInitializer tokens
                    match TokStream.npeek 2 tokens with
                    | [ T.Comma; T.CloseBrace ] ->
                        let! _, tokens = takeToken tokens
                        return ([ nextInit ], tokens)
                    | T.Comma :: _ ->
                        let! _, tokens = takeToken tokens
                        let! rest, tokens = parseInitLoop tokens
                        return (nextInit :: rest, tokens)
                    | _ -> return ([ nextInit ], tokens)
                }
            let! initList, tokens = parseInitLoop tokens
            let! tokens = expect T.CloseBrace tokens
            return (Ast.Initializer.CompoundInit initList, tokens)
        else
            let! e, tokens = parseExp 0 tokens
            return (Ast.Initializer.SingleInit e, tokens)
    }

let private parseMemberDeclaration (tokens: TokStream.TokStream) =
    result {
        let! specifiers, tokens = parseTypeSpecifierList tokens
        let! baseType = parseType specifiers
        let! decl, tokens = parseDeclarator tokens
        match decl with
        | FunDeclarator _ ->
            return! Error "Found function declarator in struct member list"
        | _ ->
            let! tokens = expect T.Semicolon tokens
            let! memberName, memberType, _params =
                processDeclarator decl baseType
            let memberResult : Ast.MemberDeclaration =
                { memberName = memberName
                  memberType = memberType }
            return (memberResult, tokens)
    }

let private parseStructDeclaration (tokens: TokStream.TokStream) =
    result {
        let! tokens = expect T.KWStruct tokens
        let! tag, tokens = parseId tokens
        let! members, tokens =
            match peekOpt tokens with
            | Some T.OpenBrace ->
                result {
                    let! _, tokens = takeToken tokens
                    let rec parseMemberLoop tokens =
                        result {
                            let! nextMember, tokens = parseMemberDeclaration tokens
                            if peekEq T.CloseBrace tokens then
                                return ([ nextMember ], tokens)
                            else
                                let! rest, tokens = parseMemberLoop tokens
                                return (nextMember :: rest, tokens)
                        }
                    let! members, tokens = parseMemberLoop tokens
                    let! tokens = expect T.CloseBrace tokens
                    return (members, tokens)
                }
            | _ -> Ok ([], tokens)
        let! tokens = expect T.Semicolon tokens
        let structResult : Ast.StructDeclaration =
            { tag = tag
              members = members }
        return (structResult, tokens)
    }

let rec private parseFunctionOrVariableDeclaration (tokens: TokStream.TokStream) =
    result {
        let! specifiers, tokens = parseSpecifierList tokens
        let! baseType, storageClass = parseTypeAndStorageClass specifiers
        let! decl, tokens = parseDeclarator tokens
        let! name, typ, paramList = processDeclarator decl baseType
        match typ with
        | Types.FunType _ ->
            let! body, tokens =
                match peekOpt tokens with
                | Some T.Semicolon ->
                    result {
                        let! _, tokens = takeToken tokens
                        return (None, tokens)
                    }
                | _ ->
                    result {
                        let! block, tokens = parseBlock tokens
                        return (Some block, tokens)
                    }
            return (Ast.FunDecl
                { name = name
                  funType = typ
                  storageClass = storageClass
                  paramList = paramList
                  body = body }, tokens)
        | _ ->
            let! init, tokens =
                if peekEq T.EqualSign tokens then
                    result {
                        let! _, tokens = takeToken tokens
                        let! init, tokens = parseInitializer tokens
                        return (Some init, tokens)
                    }
                else
                    Ok (None, tokens)
            let! tokens = expect T.Semicolon tokens
            return (Ast.VarDecl
                { name = name
                  varType = typ
                  storageClass = storageClass
                  init = init }, tokens)
    }

and private parseDeclaration (tokens: TokStream.TokStream) =
    result {
        match TokStream.npeek 3 tokens with
        | [ T.KWStruct; T.Identifier _; (T.OpenBrace | T.Semicolon) ] ->
            let! sd, tokens = parseStructDeclaration tokens
            return (Ast.StructDecl sd, tokens)
        | _ -> return! parseFunctionOrVariableDeclaration tokens
    }

and private parseForInit (tokens: TokStream.TokStream) =
    result {
        if peekIs isSpecifier tokens then
            let! decl, tokens = parseDeclaration tokens
            match decl with
            | Ast.VarDecl vd -> return (Ast.InitDecl vd, tokens)
            | _ ->
                return!
                    Error
                        "Found a function declaration in a for loop header"
        else
            let! optE, tokens = parseOptionalExp T.Semicolon tokens
            return (Ast.InitExp optE, tokens)
    }

and parseStatement (tokens: TokStream.TokStream) =
    result {
        match peekOpt tokens with
        | Some T.KWReturn ->
            let! _, tokens = takeToken tokens
            let! optExp, tokens = parseOptionalExp T.Semicolon tokens
            return (Ast.Return optExp, tokens)
        | Some T.KWIf ->
            let! _, tokens = takeToken tokens
            let! tokens = expect T.OpenParen tokens
            let! condition, tokens = parseExp 0 tokens
            let! tokens = expect T.CloseParen tokens
            let! thenClause, tokens = parseStatement tokens
            let! elseClause, tokens =
                if peekEq T.KWElse tokens then
                    result {
                        let! _, tokens = takeToken tokens
                        let! e, tokens = parseStatement tokens
                        return (Some e, tokens)
                    }
                else
                    Ok (None, tokens)
            return (Ast.If(condition, thenClause, elseClause), tokens)
        | Some T.OpenBrace ->
            let! block, tokens = parseBlock tokens
            return (Ast.Compound block, tokens)
        | Some T.KWBreak ->
            let! _, tokens = takeToken tokens
            let! tokens = expect T.Semicolon tokens
            return (Ast.Break "", tokens)
        | Some T.KWContinue ->
            let! _, tokens = takeToken tokens
            let! tokens = expect T.Semicolon tokens
            return (Ast.Continue "", tokens)
        | Some T.KWWhile ->
            let! _, tokens = takeToken tokens
            let! tokens = expect T.OpenParen tokens
            let! condition, tokens = parseExp 0 tokens
            let! tokens = expect T.CloseParen tokens
            let! body, tokens = parseStatement tokens
            return (Ast.While(condition, body, ""), tokens)
        | Some T.KWDo ->
            let! tokens = expect T.KWDo tokens
            let! body, tokens = parseStatement tokens
            let! tokens = expect T.KWWhile tokens
            let! tokens = expect T.OpenParen tokens
            let! condition, tokens = parseExp 0 tokens
            let! tokens = expect T.CloseParen tokens
            let! tokens = expect T.Semicolon tokens
            return (Ast.DoWhile(body, condition, ""), tokens)
        | Some T.KWFor ->
            let! tokens = expect T.KWFor tokens
            let! tokens = expect T.OpenParen tokens
            let! init, tokens = parseForInit tokens
            let! condition, tokens = parseOptionalExp T.Semicolon tokens
            let! post, tokens = parseOptionalExp T.CloseParen tokens
            let! body, tokens = parseStatement tokens
            return (Ast.For(init, condition, post, body, ""), tokens)
        | _ ->
            let! optExp, tokens = parseOptionalExp T.Semicolon tokens
            match optExp with
            | Some e -> return (Ast.Expression e, tokens)
            | None -> return (Ast.Null, tokens)
    }

and private parseBlockItem (tokens: TokStream.TokStream) =
    result {
        if peekIs isSpecifier tokens then
            let! decl, tokens = parseDeclaration tokens
            return (Ast.Decl decl, tokens)
        else
            let! stmt, tokens = parseStatement tokens
            return (Ast.Stmt stmt, tokens)
    }

and private parseBlock (tokens: TokStream.TokStream) =
    result {
        let! tokens = expect T.OpenBrace tokens
        let rec parseBlockItemLoop tokens =
            result {
                if peekEq T.CloseBrace tokens then
                    return ([], tokens)
                else
                    let! nextBlockItem, tokens = parseBlockItem tokens
                    let! rest, tokens = parseBlockItemLoop tokens
                    return (nextBlockItem :: rest, tokens)
            }
        let! block, tokens = parseBlockItemLoop tokens
        let! tokens = expect T.CloseBrace tokens
        return (Ast.Block block, tokens)
    }

let private parseProgram (tokens: TokStream.TokStream) =
    result {
        let rec parseDeclLoop tokens =
            result {
                if TokStream.isEmpty tokens then
                    return ([], tokens)
                else
                    let! nextDecl, tokens = parseDeclaration tokens
                    let! rest, tokens = parseDeclLoop tokens
                    return (nextDecl :: rest, tokens)
            }
        let! funDecls, _tokens = parseDeclLoop tokens
        return Ast.UntypedProgram.Program funDecls
    }

let parse tokens =
    let tokenStream = TokStream.ofList tokens
    parseProgram tokenStream
