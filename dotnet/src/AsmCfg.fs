module AsmCfg

let simplify = function
    | Assembly.Label l -> Cfg.Label l
    | Assembly.Jmp target -> Cfg.UnconditionalJump target
    | Assembly.JmpCC(_, target) -> Cfg.ConditionalJump target
    | Assembly.Ret -> Cfg.Return
    | _ -> Cfg.Other

let instructionsToCfg debugLabel instructions =
    Cfg.instructionsToCfg simplify debugLabel instructions

let cfgToInstructions g = Cfg.cfgToInstructions g
let getSuccs ndId cfg = Cfg.getSuccs ndId cfg
let getBlockValue blocknum cfg = Cfg.getBlockValue blocknum cfg
let addEdge pred succ g = Cfg.addEdge pred succ g
let removeEdge pred succ g = Cfg.removeEdge pred succ g
let updateBasicBlock idx blk g = Cfg.updateBasicBlock idx blk g
let initializeAnnotation cfg v = Cfg.initializeAnnotation cfg v
let stripAnnotations cfg = Cfg.stripAnnotations cfg
