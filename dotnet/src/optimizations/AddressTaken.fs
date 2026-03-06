module Address_taken

let analyze instrs =
    let addrTaken =
        function
        | Tacky.GetAddress { src = Tacky.Var v } -> Some v
        | _ -> None

    Set.ofList (List.choose addrTaken instrs)

let analyzeProgram (Tacky.Program tls) =
    let analyzeTl =
        function
        | Tacky.Function f -> analyze f.body
        | _ -> Set.empty

    let aliasedVarsPerFun = List.map analyzeTl tls
    List.fold Set.union Set.empty aliasedVarsPerFun