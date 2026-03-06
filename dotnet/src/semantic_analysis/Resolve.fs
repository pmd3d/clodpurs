module Resolve

open Ast.Untyped
open Types
open ResultCE

type ResolvedVar = {
    unique_name: string
    from_current_scope: bool
    has_linkage: bool
}

type ResolvedStruct = { unique_tag: string; struct_from_current_scope: bool }

// F#'s Map.map takes (key -> value -> value), unlike OCaml's which takes (value -> value)
let copyIdentifierMap (m: Map<string, ResolvedVar>) =
    Map.map (fun _ entry -> { entry with from_current_scope = false }) m

let copyStructMap m =
    Map.map
        (fun _ entry -> { entry with struct_from_current_scope = false })
        m

(* replace structure tags in type specifiers *)
let rec resolveType struct_map = function
    | Structure tag ->
        match Map.tryFind tag struct_map with
        | Some entry -> Ok (Structure entry.unique_tag)
        | None -> Error "specified undeclared structure type"
    | Pointer referenced_t ->
        resolveType struct_map referenced_t |> Result.map Pointer
    | Array (elem_type, size) ->
        resolveType struct_map elem_type |> Result.map (fun t -> Array (t, size))
    | FunType (param_types, ret_type) ->
        result {
            let! resolved_param_types = resultTraverse (resolveType struct_map) param_types
            let! resolved_ret_type = resolveType struct_map ret_type
            return FunType (resolved_param_types, resolved_ret_type)
        }
    | t -> Ok t

let rec resolveExp struct_map id_map = function
    | Exp.Assignment (left, right) ->
        result {
            let! l = resolveExp struct_map id_map left
            let! r = resolveExp struct_map id_map right
            return Exp.Assignment (l, r)
        }
    | Exp.Var v ->
        match Map.tryFind v id_map with
        | Some entry -> Ok (Exp.Var entry.unique_name)
        | None -> Error (sprintf "Undeclared variable %s" v)
    | Exp.Cast (target_type, e) ->
        result {
            let! resolved_type = resolveType struct_map target_type
            let! resolved_e = resolveExp struct_map id_map e
            return Exp.Cast (resolved_type, resolved_e)
        }
    | Exp.Unary (op, e) ->
        resolveExp struct_map id_map e |> Result.map (fun e' -> Exp.Unary (op, e'))
    | Exp.Binary (op, e1, e2) ->
        result {
            let! e1' = resolveExp struct_map id_map e1
            let! e2' = resolveExp struct_map id_map e2
            return Exp.Binary (op, e1', e2')
        }
    | Exp.Conditional (condition, then_result, else_result) ->
        result {
            let! c = resolveExp struct_map id_map condition
            let! t = resolveExp struct_map id_map then_result
            let! e = resolveExp struct_map id_map else_result
            return Exp.Conditional (c, t, e)
        }
    | Exp.FunCall (f, args) ->
        match Map.tryFind f id_map with
        | Some entry ->
            resultTraverse (resolveExp struct_map id_map) args
            |> Result.map (fun resolved_args -> Exp.FunCall (entry.unique_name, resolved_args))
        | None -> Error "Undeclared function!"
    | Exp.Dereference inner ->
        resolveExp struct_map id_map inner |> Result.map Exp.Dereference
    | Exp.AddrOf inner ->
        resolveExp struct_map id_map inner |> Result.map Exp.AddrOf
    | Exp.Subscript (ptr, index) ->
        result {
            let! p = resolveExp struct_map id_map ptr
            let! i = resolveExp struct_map id_map index
            return Exp.Subscript (p, i)
        }
    | Exp.SizeOf e ->
        resolveExp struct_map id_map e |> Result.map Exp.SizeOf
    | Exp.SizeOfT t ->
        resolveType struct_map t |> Result.map Exp.SizeOfT
    | Exp.Dot (strct, mbr) ->
        resolveExp struct_map id_map strct |> Result.map (fun s -> Exp.Dot (s, mbr))
    | Exp.Arrow (strct, mbr) ->
        resolveExp struct_map id_map strct |> Result.map (fun s -> Exp.Arrow (s, mbr))
    | (Exp.Constant _ | Exp.String _) as c -> Ok c

let resolveOptionalExp struct_map id_map = function
    | Some e -> resolveExp struct_map id_map e |> Result.map Some
    | None -> Ok None

let resolveLocalVarHelper counter id_map name storage_class =
    result {
        do! match Map.tryFind name id_map with
            | Some { from_current_scope = true; has_linkage = has_linkage } ->
                if not (has_linkage && storage_class = Some Ast.StorageClass.Extern) then
                    Error "Duplicate variable declaration"
                else Ok ()
            | _ -> Ok ()
        let counter', entry =
            if storage_class = Some Ast.StorageClass.Extern then
                (counter, { unique_name = name; from_current_scope = true; has_linkage = true })
            else
                let counter', unique_name = UniqueIds.makeNamedTemporary name counter
                (counter', { unique_name = unique_name; from_current_scope = true; has_linkage = false })
        let new_map = Map.add name entry id_map
        return (counter', new_map, entry.unique_name)
    }

let rec resolveInitializer struct_map id_map = function
    | Initializer.SingleInit e ->
        resolveExp struct_map id_map e |> Result.map Initializer.SingleInit
    | Initializer.CompoundInit inits ->
        resultTraverse (resolveInitializer struct_map id_map) inits
        |> Result.map Initializer.CompoundInit

let resolveLocalVarDeclaration counter struct_map id_map
        { name = name; varType = var_type; init = init; storageClass = storage_class } =
    result {
        let! counter', new_id_map, unique_name =
            resolveLocalVarHelper counter id_map name storage_class
        let! resolved_type = resolveType struct_map var_type
        let! resolved_init =
            match init with
            | Some i -> resolveInitializer struct_map new_id_map i |> Result.map Some
            | None -> Ok None
        return ( counter', new_id_map,
          {
              name = unique_name
              varType = resolved_type
              init = resolved_init
              storageClass = storage_class
          } )
    }

let resolveForInit counter struct_map id_map = function
    | InitExp e ->
        resolveOptionalExp struct_map id_map e
        |> Result.map (fun e' -> (counter, id_map, InitExp e'))
    | InitDecl d ->
        resolveLocalVarDeclaration counter struct_map id_map d
        |> Result.map (fun (counter', new_id_map, resolved_decl) ->
            (counter', new_id_map, InitDecl resolved_decl))

let rec resolveStatement counter struct_map id_map = function
    | Return e ->
        resolveOptionalExp struct_map id_map e
        |> Result.map (fun resolved_e -> (counter, Return resolved_e))
    | Expression e ->
        resolveExp struct_map id_map e
        |> Result.map (fun e' -> (counter, Expression e'))
    | Statement.If (condition, then_clause, else_clause) ->
        result {
            let! counter', then' = resolveStatement counter struct_map id_map then_clause
            let! counter'', else' =
                match else_clause with
                | Some e ->
                    resolveStatement counter' struct_map id_map e
                    |> Result.map (fun (c, e') -> (c, Some e'))
                | None -> Ok (counter', None)
            let! resolved_condition = resolveExp struct_map id_map condition
            return (counter'',
                Statement.If (resolved_condition, then', else'))
        }
    | While (condition, body, id) ->
        result {
            let! counter', body' = resolveStatement counter struct_map id_map body
            let! resolved_condition = resolveExp struct_map id_map condition
            return (counter', While (resolved_condition, body', id))
        }
    | DoWhile (body, condition, id) ->
        result {
            let! counter', body' = resolveStatement counter struct_map id_map body
            let! resolved_condition = resolveExp struct_map id_map condition
            return (counter', DoWhile (body', resolved_condition, id))
        }
    | For (init, condition, post, body, id) ->
        result {
            let id_map1 = copyIdentifierMap id_map
            let struct_map1 = copyStructMap struct_map
            let! counter', id_map2, resolved_init = resolveForInit counter struct_map1 id_map1 init
            let! counter'', body' = resolveStatement counter' struct_map1 id_map2 body
            let! resolved_condition = resolveOptionalExp struct_map1 id_map2 condition
            let! resolved_post = resolveOptionalExp struct_map1 id_map2 post
            return (counter'',
                For (resolved_init, resolved_condition, resolved_post, body', id))
        }
    | Compound block ->
        result {
            let new_variable_map = copyIdentifierMap id_map
            let new_struct_map = copyStructMap struct_map
            let! counter', block' = resolveBlock counter new_struct_map new_variable_map block
            return (counter', Compound block')
        }
    | (Null | Break _ | Continue _) as s -> Ok (counter, s)

and resolveBlockItem counter (struct_map, id_map) = function
    | Stmt s ->
        resolveStatement counter struct_map id_map s
        |> Result.map (fun (counter', resolved_s) ->
            (counter', (struct_map, id_map), Stmt resolved_s))
    | Decl d ->
        resolveLocalDeclaration counter struct_map id_map d
        |> Result.map (fun (counter', new_maps, resolved_d) ->
            (counter', new_maps, Decl resolved_d))

and resolveBlock counter struct_map id_map (Block items) =
    resultFold (fun (c, maps, acc) item ->
        resolveBlockItem c maps item
        |> Result.map (fun (c', maps', item') -> (c', maps', acc @ [item'])))
        (counter, (struct_map, id_map), []) items
    |> Result.map (fun (counter', _final_maps, resolved_items) ->
        (counter', Block resolved_items))

and resolveLocalDeclaration counter struct_map id_map = function
    | VarDecl vd ->
        resolveLocalVarDeclaration counter struct_map id_map vd
        |> Result.map (fun (counter', new_id_map, resolved_vd) ->
            (counter', (struct_map, new_id_map), VarDecl resolved_vd))
    | FunDecl { body = Some _ } ->
        Error "nested function definitions are not allowed"
    | FunDecl { storageClass = Some Ast.StorageClass.Static } ->
        Error "static keyword not allowed on local function declarations"
    | FunDecl fd ->
        resolveFunctionDeclaration counter struct_map id_map fd
        |> Result.map (fun (counter', new_id_map, resolved_fd) ->
            (counter', (struct_map, new_id_map), FunDecl resolved_fd))
    | StructDecl sd ->
        resolveStructureDeclaration counter struct_map sd
        |> Result.map (fun (counter', new_struct_map, resolved_sd) ->
            (counter', (new_struct_map, id_map), StructDecl resolved_sd))

and resolveParams counter id_map param_names =
    resultFold (fun (c, m, acc) param_name ->
        resolveLocalVarHelper c m param_name None
        |> Result.map (fun (c', m', unique) -> (c', m', acc @ [unique])))
        (counter, id_map, []) param_names

and resolveFunctionDeclaration counter struct_map id_map fn =
    result {
        do! match Map.tryFind fn.name id_map with
            | Some { from_current_scope = true; has_linkage = false } ->
                Error "Duplicate declaration"
            | _ -> Ok ()
        let! resolved_type = resolveType struct_map fn.funType
        let new_entry =
            { unique_name = fn.name; from_current_scope = true; has_linkage = true }
        let new_id_map = Map.add fn.name new_entry id_map
        let inner_id_map = copyIdentifierMap new_id_map
        let! counter', inner_id_map1, resolved_params =
            resolveParams counter inner_id_map fn.paramList
        let inner_struct_map = copyStructMap struct_map
        let! counter'', resolved_body =
            match fn.body with
            | Some body ->
                resolveBlock counter' inner_struct_map inner_id_map1 body
                |> Result.map (fun (c, b) -> (c, Some b))
            | None -> Ok (counter', None)
        return ( counter'', new_id_map,
          {
              fn with
                  funType = resolved_type
                  paramList = resolved_params
                  body = resolved_body
          } )
    }

and resolveStructureDeclaration counter struct_map { tag = tag; members = members } =
    let counter', new_map, resolved_tag =
        match Map.tryFind tag struct_map with
        | Some { unique_tag = unique_tag; struct_from_current_scope = true } ->
            (counter, struct_map, unique_tag)
        | _ ->
            let counter', unique_tag = UniqueIds.makeNamedTemporary tag counter
            let entry = { unique_tag = unique_tag; struct_from_current_scope = true }
            (counter', Map.add tag entry struct_map, unique_tag)
    let resolveMember m =
        resolveType new_map m.memberType
        |> Result.map (fun t -> { m with memberType = t })
    resultTraverse resolveMember members
    |> Result.map (fun resolved_members ->
        (counter', new_map, { tag = resolved_tag; members = resolved_members }))

let resolveFileScopeVariableDeclaration struct_map id_map
        ({ name = name; varType = var_type } as vd: VariableDeclaration) =
    resolveType struct_map var_type
    |> Result.map (fun resolved_type ->
        let resolved_vd = { vd with varType = resolved_type }
        let new_map =
            Map.add name
                { unique_name = name; from_current_scope = true; has_linkage = true }
                id_map
        (new_map, resolved_vd))

let resolveGlobalDeclaration counter (struct_map, id_map) = function
    | FunDecl fd ->
        resolveFunctionDeclaration counter struct_map id_map fd
        |> Result.map (fun (counter', id_map1, fd) ->
            (counter', (struct_map, id_map1), FunDecl fd))
    | VarDecl vd ->
        resolveFileScopeVariableDeclaration struct_map id_map vd
        |> Result.map (fun (id_map1, resolved_vd) ->
            (counter, (struct_map, id_map1), VarDecl resolved_vd))
    | StructDecl sd ->
        resolveStructureDeclaration counter struct_map sd
        |> Result.map (fun (counter', struct_map1, resolved_sd) ->
            (counter', (struct_map1, id_map), StructDecl resolved_sd))

let resolve counter (Program decls) =
    resultFold (fun (c, maps, acc) d ->
        resolveGlobalDeclaration c maps d
        |> Result.map (fun (c', maps', d') -> (c', maps', acc @ [d'])))
        (counter, (Map.empty, Map.empty), []) decls
    |> Result.map (fun (counter', _, resolved_decls) ->
        (counter', Program resolved_decls))
