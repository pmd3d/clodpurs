module AsmCfg where

import Prelude

import Assembly (AsmInstruction(..))
import Cfg as Cfg
import CompilerError (CompilerError)
import Data.Either (Either)
import Data.List (List)

simplify :: AsmInstruction -> Cfg.SimpleInstr
simplify (Label l) = Cfg.Label l
simplify (Jmp target) = Cfg.UnconditionalJump target
simplify (JmpCC _ target) = Cfg.ConditionalJump target
simplify Ret = Cfg.Return
simplify _ = Cfg.Other

instructionsToCfg :: String -> List AsmInstruction -> Either CompilerError (Cfg.ControlFlowGraph Unit AsmInstruction)
instructionsToCfg debugLabel instructions =
  Cfg.instructionsToCfg simplify debugLabel instructions

cfgToInstructions :: forall v. Cfg.ControlFlowGraph v AsmInstruction -> List AsmInstruction
cfgToInstructions = Cfg.cfgToInstructions

getSuccs :: forall v. Cfg.NodeId -> Cfg.ControlFlowGraph v AsmInstruction -> Either CompilerError (List Cfg.NodeId)
getSuccs = Cfg.getSuccs

getBlockValue :: forall v. Int -> Cfg.ControlFlowGraph v AsmInstruction -> Either CompilerError v
getBlockValue = Cfg.getBlockValue

addEdge :: forall v. Cfg.NodeId -> Cfg.NodeId -> Cfg.ControlFlowGraph v AsmInstruction -> Either CompilerError (Cfg.ControlFlowGraph v AsmInstruction)
addEdge = Cfg.addEdge

removeEdge :: forall v. Cfg.NodeId -> Cfg.NodeId -> Cfg.ControlFlowGraph v AsmInstruction -> Either CompilerError (Cfg.ControlFlowGraph v AsmInstruction)
removeEdge = Cfg.removeEdge

updateBasicBlock :: forall v. Int -> Cfg.BasicBlock v AsmInstruction -> Cfg.ControlFlowGraph v AsmInstruction -> Cfg.ControlFlowGraph v AsmInstruction
updateBasicBlock = Cfg.updateBasicBlock

initializeAnnotation :: forall v1 v2. Cfg.ControlFlowGraph v1 AsmInstruction -> v2 -> Cfg.ControlFlowGraph v2 AsmInstruction
initializeAnnotation = Cfg.initializeAnnotation

stripAnnotations :: forall v. Cfg.ControlFlowGraph v AsmInstruction -> Cfg.ControlFlowGraph Unit AsmInstruction
stripAnnotations = Cfg.stripAnnotations
