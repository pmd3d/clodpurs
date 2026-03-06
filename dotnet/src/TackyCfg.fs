module TackyCfg

let simplify = function
    | Tacky.Label l -> Cfg.Label l
    | Tacky.Jump target -> Cfg.UnconditionalJump target
    | Tacky.JumpIfZero(_, target) -> Cfg.ConditionalJump target
    | Tacky.JumpIfNotZero(_, target) -> Cfg.ConditionalJump target
    | Tacky.Return _ -> Cfg.Return
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
