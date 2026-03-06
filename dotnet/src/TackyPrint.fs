module Tacky_print

open Tacky

// -- Helpers to replace OCaml's Format module --

let inline write (out: System.IO.TextWriter) (s: string) = out.Write(s)

let ppPrintList (ppSep: System.IO.TextWriter -> unit)
                  (ppItem: System.IO.TextWriter -> 'a -> unit)
                  (out: System.IO.TextWriter)
                  (items: 'a list) =
    items |> List.iteri (fun i item ->
        if i > 0 then ppSep out
        ppItem out item)

let commaSep (out: System.IO.TextWriter) = write out ", "

let ppInitList (out: System.IO.TextWriter) (init_list: Initializers.StaticInit list) =
    write out "{"
    ppPrintList commaSep Initializers.ppStaticInit out init_list
    write out "}"

let ppUnaryOperator (out: System.IO.TextWriter) = function
    | Complement -> write out "~"
    | Negate -> write out "-"
    | Not -> write out "!"

// optional escape_brackets argument lets us escape < and > when instructions
// appear in HTML-style tables in graphviz
let ppBinaryOperator (escape_brackets: bool) (out: System.IO.TextWriter) = function
    | Add -> write out "+"
    | Subtract -> write out "-"
    | Multiply -> write out "*"
    | Divide -> write out "/"
    | Mod -> write out "%"
    | Equal -> write out "=="
    | NotEqual -> write out "!="
    | LessThan ->
        let s = if escape_brackets then "&lt;" else "<"
        write out s
    | LessOrEqual ->
        let s = if escape_brackets then "&lt;=" else "<="
        write out s
    | GreaterThan ->
        let s = if escape_brackets then "&gt;" else ">"
        write out s
    | GreaterOrEqual ->
        let s = if escape_brackets then "&gt;=" else ">="
        write out s

let constToString = function
    | Const.ConstInt i -> sprintf "%d" i
    | Const.ConstLong l -> sprintf "%d" l + "l"
    | Const.ConstUInt ui -> sprintf "%u" ui + "u"
    | Const.ConstULong ul -> sprintf "%u" ul + "ul"
    | Const.ConstDouble d -> sprintf "%g" d
    | Const.ConstChar c -> sprintf "%d" c
    | Const.ConstUChar uc -> sprintf "%u" uc

let ppTackyVal (out: System.IO.TextWriter) = function
    | Constant i -> write out (constToString i)
    | Var s -> write out s

let ppTackyValList (out: System.IO.TextWriter) (vals: TackyVal list) =
    ppPrintList commaSep ppTackyVal out vals

let ppStringList (out: System.IO.TextWriter) (strs: string list) =
    ppPrintList commaSep (fun o s -> write o s) out strs

let ppInstruction (escape_brackets: bool) (out: System.IO.TextWriter) = function
    | Return None -> write out "Return"
    | Return (Some v) ->
        write out "Return("
        ppTackyVal out v
        write out ")"
    | Unary { op = op; src = src; dst = dst } ->
        ppTackyVal out dst
        write out " = "
        ppUnaryOperator out op
        ppTackyVal out src
    | Binary { op = op; src1 = src1; src2 = src2; dst = dst } ->
        ppTackyVal out dst
        write out " = "
        ppTackyVal out src1
        write out " "
        ppBinaryOperator escape_brackets out op
        write out " "
        ppTackyVal out src2
    | Copy { src = src; dst = dst } ->
        ppTackyVal out dst
        write out " = "
        ppTackyVal out src
    | Jump s ->
        write out (sprintf "Jump(%s)" s)
    | JumpIfZero (cond, target) ->
        write out "JumpIfZero("
        ppTackyVal out cond
        write out (sprintf ", %s)" target)
    | JumpIfNotZero (cond, target) ->
        write out "JumpIfNotZero("
        ppTackyVal out cond
        write out (sprintf ", %s)" target)
    | Label s ->
        out.WriteLine()
        write out (sprintf "%s:" s)
    | FunCall { f = f; args = args; dst = None } ->
        write out (sprintf "%s(" f)
        ppTackyValList out args
        write out ")"
    | FunCall { f = f; args = args; dst = Some dst } ->
        ppTackyVal out dst
        write out (sprintf " = %s(" f)
        ppTackyValList out args
        write out ")"
    | SignExtend { src = src; dst = dst } ->
        ppTackyVal out dst
        write out " = SignExtend("
        ppTackyVal out src
        write out ")"
    | ZeroExtend { src = src; dst = dst } ->
        ppTackyVal out dst
        write out " = ZeroExtend("
        ppTackyVal out src
        write out ")"
    | Truncate { src = src; dst = dst } ->
        ppTackyVal out dst
        write out " = Truncate("
        ppTackyVal out src
        write out ")"
    | DoubleToInt { src = src; dst = dst } ->
        ppTackyVal out dst
        write out " = DoubleToInt("
        ppTackyVal out src
        write out ")"
    | DoubleToUInt { src = src; dst = dst } ->
        ppTackyVal out dst
        write out " = DoubleToUInt("
        ppTackyVal out src
        write out ")"
    | IntToDouble { src = src; dst = dst } ->
        ppTackyVal out dst
        write out " = IntToDouble("
        ppTackyVal out src
        write out ")"
    | UIntToDouble { src = src; dst = dst } ->
        ppTackyVal out dst
        write out " = UIntToDouble("
        ppTackyVal out src
        write out ")"
    | GetAddress { src = src; dst = dst } ->
        ppTackyVal out dst
        write out " = GetAddress("
        ppTackyVal out src
        write out ")"
    | Load v ->
        ppTackyVal out v.dst
        write out " = Load("
        ppTackyVal out v.src_ptr
        write out ")"
    | Store v ->
        write out "*("
        ppTackyVal out v.dst_ptr
        write out ") = "
        ppTackyVal out v.src
    | AddPtr { ptr = ptr; index = index; scale = scale; dst = dst } ->
        ppTackyVal out dst
        write out " = "
        ppTackyVal out ptr
        write out " + "
        ppTackyVal out index
        write out (sprintf " * %d" scale)
    | CopyToOffset { src = src; dst = dst; offset = offset } ->
        write out (sprintf "%s[offset = %d] = " dst offset)
        ppTackyVal out src
    | CopyFromOffset { src = src; offset = offset; dst = dst } ->
        ppTackyVal out dst
        write out (sprintf " = %s[offset = %d]" src offset)

let ppFunctionDefinition (escape_brackets: bool) (isGlobal: bool) (name: string)
                           (paramList: string list) (out: System.IO.TextWriter)
                           (body: TackyInstruction list) =
    if isGlobal then write out "global "
    write out (sprintf "%s(" name)
    ppStringList out paramList
    write out "):"
    out.WriteLine()
    write out "    "
    ppPrintList
        (fun o -> o.WriteLine(); write o "    ")
        (ppInstruction escape_brackets)
        out body

let ppTl (escape_brackets: bool) (out: System.IO.TextWriter) = function
    | Function { name = name; isGlobal = isGlobal; paramList = paramList; body = body } ->
        ppFunctionDefinition escape_brackets isGlobal name paramList out body
    | StaticVariable { isGlobal = isGlobal; name = name; init = init; t = t } ->
        if isGlobal then write out "global "
        Types.pp out t
        write out (sprintf " %s = " name)
        ppInitList out init
    | StaticConstant { name = name; init = init; t = t } ->
        write out "const "
        Types.pp out t
        write out (sprintf " %s = " name)
        Initializers.ppStaticInit out init

let ppProgram (escape_brackets: bool) (out: System.IO.TextWriter) (Program tls) =
    ppPrintList
        (fun o -> o.WriteLine(); o.WriteLine())
        (ppTl escape_brackets)
        out tls
    out.WriteLine()
    out.Flush()

let debugPrintTacky (debug: bool) (counter: UniqueIds.Counter) (src_filename: string) tacky_prog =
    if debug then
        let counter', lbl = UniqueIds.makeLabel (System.IO.Path.GetFileNameWithoutExtension(src_filename)) counter
        let tacky_file = lbl + ".debug.tacky"
        use sw = new System.IO.StringWriter()
        ppProgram false (sw :> System.IO.TextWriter) tacky_prog
        (counter', Some (tacky_file, sw.ToString()))
    else
        (counter, None)