module Label_loops

open Ast.Untyped
open ResultCE

let rec labelStatement counter current_label = function
  | Break _ ->
      match current_label with
      | Some l -> Ok (counter, Break l)
      | None -> Error "Break outside of loop"
  | Continue _ ->
      match current_label with
      | Some l -> Ok (counter, Continue l)
      | None -> Error "Continue outside of loop"
  | While(condition, body, _id) ->
      let counter', new_id = UniqueIds.makeLabel "while" counter
      labelStatement counter' (Some new_id) body
      |> Result.map (fun (counter'', body') ->
          (counter'', Statement.While(condition = condition, body = body', id = new_id)))
  | DoWhile(body, condition, _id) ->
      let counter', new_id = UniqueIds.makeLabel "do_while" counter
      labelStatement counter' (Some new_id) body
      |> Result.map (fun (counter'', body') ->
          (counter'', DoWhile(body = body', condition = condition, id = new_id)))
  | For(init, condition, post, body, _id) ->
      let counter', new_id = UniqueIds.makeLabel "for" counter
      labelStatement counter' (Some new_id) body
      |> Result.map (fun (counter'', body') ->
          (counter'', Statement.For(init = init, condition = condition, post = post, body = body', id = new_id)))
  | Compound blk ->
      labelBlock counter current_label blk
      |> Result.map (fun (counter', blk') -> (counter', Compound blk'))
  | If(condition, thenClause, elseClause) ->
      result {
          let! counter', thenClause' = labelStatement counter current_label thenClause
          let! counter'', elseClause' =
              match elseClause with
              | Some e ->
                  labelStatement counter' current_label e
                  |> Result.map (fun (c, e') -> (c, Some e'))
              | None -> Ok (counter', None)
          return (counter'', If(condition = condition, thenClause = thenClause', elseClause = elseClause'))
      }
  | (Null | Return _ | Expression _) as s -> Ok (counter, s)

and labelBlockItem counter current_label = function
  | Stmt s ->
      labelStatement counter current_label s
      |> Result.map (fun (counter', s') -> (counter', Stmt s'))
  | decl -> Ok (counter, decl)

and labelBlock counter current_label (Block b) =
  resultFold (fun (c, acc) item ->
      labelBlockItem c current_label item
      |> Result.map (fun (c', item') -> (c', acc @ [item'])))
      (counter, []) b
  |> Result.map (fun (counter', items) -> (counter', Block items))

let labelDecl counter = function
  | FunDecl fn ->
      match fn.body with
      | Some body ->
          labelBlock counter None body
          |> Result.map (fun (counter', body') ->
              (counter', FunDecl { fn with body = Some body' }))
      | None -> Ok (counter, FunDecl fn)
  | var_decl -> Ok (counter, var_decl)

let labelLoops counter (Program decls) =
    resultFold (fun (c, acc) d ->
        labelDecl c d
        |> Result.map (fun (c', d') -> (c', acc @ [d'])))
        (counter, []) decls
    |> Result.map (fun (counter', decls') -> (counter', Program decls'))
