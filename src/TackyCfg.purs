module TackyCfg where

import Prelude

import Cfg as Cfg
import CompilerError (CompilerError)
import Data.Either (Either)
import Data.List (List)
import Tacky (TackyInstruction(..))

simplify :: TackyInstruction -> Cfg.SimpleInstr
simplify (TLabel l) = Cfg.Label l
simplify (Jump target) = Cfg.UnconditionalJump target
simplify (JumpIfZero _ target) = Cfg.ConditionalJump target
simplify (JumpIfNotZero _ target) = Cfg.ConditionalJump target
simplify (Return _) = Cfg.Return
simplify _ = Cfg.Other

instructionsToCfg :: String -> List TackyInstruction -> Either CompilerError (Cfg.ControlFlowGraph Unit TackyInstruction)
instructionsToCfg debugLabel instructions =
  Cfg.instructionsToCfg simplify debugLabel instructions

cfgToInstructions :: forall v. Cfg.ControlFlowGraph v TackyInstruction -> List TackyInstruction
cfgToInstructions = Cfg.cfgToInstructions

getSuccs :: forall v. Cfg.NodeId -> Cfg.ControlFlowGraph v TackyInstruction -> Either CompilerError (List Cfg.NodeId)
getSuccs = Cfg.getSuccs

getBlockValue :: forall v. Int -> Cfg.ControlFlowGraph v TackyInstruction -> Either CompilerError v
getBlockValue = Cfg.getBlockValue

addEdge :: forall v. Cfg.NodeId -> Cfg.NodeId -> Cfg.ControlFlowGraph v TackyInstruction -> Either CompilerError (Cfg.ControlFlowGraph v TackyInstruction)
addEdge = Cfg.addEdge

removeEdge :: forall v. Cfg.NodeId -> Cfg.NodeId -> Cfg.ControlFlowGraph v TackyInstruction -> Either CompilerError (Cfg.ControlFlowGraph v TackyInstruction)
removeEdge = Cfg.removeEdge

updateBasicBlock :: forall v. Int -> Cfg.BasicBlock v TackyInstruction -> Cfg.ControlFlowGraph v TackyInstruction -> Cfg.ControlFlowGraph v TackyInstruction
updateBasicBlock = Cfg.updateBasicBlock

initializeAnnotation :: forall v1 v2. Cfg.ControlFlowGraph v1 TackyInstruction -> v2 -> Cfg.ControlFlowGraph v2 TackyInstruction
initializeAnnotation = Cfg.initializeAnnotation

stripAnnotations :: forall v. Cfg.ControlFlowGraph v TackyInstruction -> Cfg.ControlFlowGraph Unit TackyInstruction
stripAnnotations = Cfg.stripAnnotations
