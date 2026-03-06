module Reg_set

type RegSet = Set<Assembly.AsmReg>

let empty: RegSet = Set.empty
let singleton x: RegSet = Set.singleton x
let add x (s: RegSet): RegSet = Set.add x s
let remove x (s: RegSet): RegSet = Set.remove x s
let mem x (s: RegSet) = Set.contains x s
let union (s1: RegSet) (s2: RegSet): RegSet = Set.union s1 s2
let inter (s1: RegSet) (s2: RegSet): RegSet = Set.intersect s1 s2
let diff (s1: RegSet) (s2: RegSet): RegSet = Set.difference s1 s2
let isEmpty (s: RegSet) = Set.isEmpty s
let cardinal (s: RegSet) = Set.count s
let iter f (s: RegSet) = Set.iter f s
let fold f (s: RegSet) acc = Set.fold f acc s
let map f (s: RegSet): RegSet = Set.map f s
let filter f (s: RegSet): RegSet = Set.filter f s
let exists f (s: RegSet) = Set.exists f s
let forAll f (s: RegSet) = Set.forall f s
let elements (s: RegSet) = Set.toList s
let ofList (l: Assembly.AsmReg list): RegSet = Set.ofList l
let subset (s1: RegSet) (s2: RegSet) = Set.isSubset s1 s2
let choose (s: RegSet) = Set.minElement s