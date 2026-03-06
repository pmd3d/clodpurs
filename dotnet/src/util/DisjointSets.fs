module DisjointSets

type DisjointSet<'a when 'a: comparison> = Map<'a, 'a>

let init<'a when 'a: comparison> : DisjointSet<'a> = Map.empty

let union (x: 'a) (y: 'a) (disjSets: DisjointSet<'a>) : DisjointSet<'a> =
    Map.add x y disjSets

let rec find (x: 'a) (disjSets: DisjointSet<'a>) : 'a =
    match Map.tryFind x disjSets with
    | Some mappedTo -> find mappedTo disjSets
    | None -> x

let isEmpty (disjSets: DisjointSet<'a>) : bool = Map.isEmpty disjSets