module TypeTable

(* structure type definitions *)

type MemberDef = { member_type: Types.CType; offset: int }

type StructDef = {
    alignment: int
    size: int
    members: Map<string, MemberDef>
}

type TypeTableMap = Map<string, StructDef>

let empty : TypeTableMap = Map.empty

let addStructDefinition tag structDef (tt: TypeTableMap) : TypeTableMap =
    Map.add tag structDef tt

let mem tag (tt: TypeTableMap) = Map.containsKey tag tt
let tryFind tag (tt: TypeTableMap) = Map.tryFind tag tt
let find tag (tt: TypeTableMap) =
    match Map.tryFind tag tt with
    | Some v -> Ok v
    | None -> Error (CompilerError.InternalError ("type table missing tag " + tag))

let getMembers tag (tt: TypeTableMap) =
    find tag tt
    |> Result.map (fun structDef ->
        let compareOffset m1 m2 = compare m1.offset m2.offset
        structDef.members
        |> Map.toList
        |> List.map snd
        |> List.sortWith compareOffset)

let getMemberTypes tag (tt: TypeTableMap) =
    getMembers tag tt |> Result.map (List.map (fun m -> m.member_type))
