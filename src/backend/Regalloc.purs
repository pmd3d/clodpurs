module Regalloc where

import Prelude

import Assembly (AsmInstruction(..), AsmOperand(..), AsmProgram(..), AsmReg(..), AsmTopLevel(..), AsmType(..))
import Data.BigInt as BigInt
import AssemblySymbols (AsmSymbolTableMap)
import AssemblySymbols as AssemblySymbols
import AsmCfg as AsmCfg
import BackwardDataflow as BackwardDataflow
import Cfg as Cfg
import CompilerError (CompilerError(..))
import Data.Either (Either(..))
import Data.Int (toNumber)
import Data.List (List(..), (:), concatMap, filter, mapMaybe)
import Data.List as List
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Number (infinity)
import Data.Set (Set)
import Data.Set as Set
import Data.String.CodeUnits as SCU
import Data.Tuple (Tuple(..), snd)
import DisjointSets as DisjointSets
import ListUtil as ListUtil
import RegSet as RegSet
import ResultUtil (resultFold, resultTraverse)
import UniqueIds as UniqueIds

-- Type aliases
type OperandSet = Set AsmOperand
type StringMap v = Map String v

-- Configuration type to replace the OCaml functor argument module
type RegTypeOps =
  { suffix :: String
  , all_hardregs :: List AsmReg
  , caller_saved_regs :: List AsmReg
  , pseudo_is_current_type :: AsmSymbolTableMap -> String -> Either CompilerError Boolean
  }

-- Node type for the interference graph
type AllocNode =
  { id :: AsmOperand
  , neighbors :: OperandSet
  , spillCost :: Number
  , color :: Maybe Int
  , pruned :: Boolean
  }

-- Type alias for the allocate function signature
type AllocateFn =
  Boolean ->
  UniqueIds.Counter ->
  AsmSymbolTableMap ->
  String ->
  Set String ->
  List AsmInstruction ->
  Either CompilerError (Tuple UniqueIds.Counter (Tuple AsmSymbolTableMap (Tuple (List AsmInstruction) (List (Tuple String String)))))

-- extract all operands from an instruction
getOperands :: AsmInstruction -> Either CompilerError (List AsmOperand)
getOperands = case _ of
  Mov _ src dst -> Right (src : dst : Nil)
  Movsx i -> Right (i.src : i.dst : Nil)
  MovZeroExtend zx -> Right (zx.src : zx.dst : Nil)
  Lea src dst -> Right (src : dst : Nil)
  Cvttsd2si _ src dst -> Right (src : dst : Nil)
  Cvtsi2sd _ src dst -> Right (src : dst : Nil)
  Unary _ _ op -> Right (op : Nil)
  Binary b -> Right (b.src : b.dst : Nil)
  Cmp _ v1 v2 -> Right (v1 : v2 : Nil)
  Idiv _ op -> Right (op : Nil)
  Div _ op -> Right (op : Nil)
  SetCC _ op -> Right (op : Nil)
  Push op -> Right (op : Nil)
  Label _ -> Right Nil
  Call _ -> Right Nil
  Ret -> Right Nil
  Cdq _ -> Right Nil
  JmpCC _ _ -> Right Nil
  Jmp _ -> Right Nil
  Pop _ -> Left (InternalError "Internal error")

-- map function f over all the operands in an instruction
replaceOps :: (AsmOperand -> AsmOperand) -> AsmInstruction -> Either CompilerError AsmInstruction
replaceOps f = case _ of
  Mov t src dst -> Right (Mov t (f src) (f dst))
  Movsx sx -> Right (Movsx (sx { dst = f sx.dst, src = f sx.src }))
  MovZeroExtend zx -> Right (MovZeroExtend (zx { dst = f zx.dst, src = f zx.src }))
  Lea src dst -> Right (Lea (f src) (f dst))
  Cvttsd2si t src dst -> Right (Cvttsd2si t (f src) (f dst))
  Cvtsi2sd t src dst -> Right (Cvtsi2sd t (f src) (f dst))
  Unary operator t operand -> Right (Unary operator t (f operand))
  Binary b -> Right (Binary (b { dst = f b.dst, src = f b.src }))
  Cmp code v1 v2 -> Right (Cmp code (f v1) (f v2))
  Idiv t v -> Right (Idiv t (f v))
  Div t v -> Right (Div t (f v))
  SetCC code dst -> Right (SetCC code (f dst))
  Push v -> Right (Push (f v))
  i@(Label _) -> Right i
  i@(Call _) -> Right i
  i@Ret -> Right i
  i@(Cdq _) -> Right i
  i@(Jmp _) -> Right i
  i@(JmpCC _ _) -> Right i
  Pop _ -> Left (InternalError "Shouldn't use this yet")

cleanupMovs :: List AsmInstruction -> List AsmInstruction
cleanupMovs instructions =
  let isRedundantMov = case _ of
        Mov _ src dst | src == dst -> true
        _ -> false
  in filter (\i -> not (isRedundantMov i)) instructions

-- Helper to mimic OCaml's List.foldLeftMap
foldLeftMap :: forall acc a b. (acc -> a -> Tuple acc b) -> acc -> List a -> Tuple acc (List b)
foldLeftMap f acc0 xs =
  let go acc result Nil = Tuple acc (List.reverse result)
      go acc result (Cons x rest) =
        let (Tuple acc' y) = f acc x
        in go acc' (y : result) rest
  in go acc0 Nil xs

lookupNode :: Map AsmOperand AllocNode -> AsmOperand -> Either CompilerError AllocNode
lookupNode graph nodeId =
  case Map.lookup nodeId graph of
    Just nd -> Right nd
    Nothing -> Left (InternalError ("node not found in graph: " <> showOperand nodeId))

showOperand :: AsmOperand -> String
showOperand (Reg r) = show r
showOperand (Pseudo p) = "(Pseudo " <> p <> ")"
showOperand (Imm i) = "(Imm " <> BigInt.toString i <> ")"
showOperand (Memory r off) = "(Memory " <> show r <> " " <> show off <> ")"
showOperand (Data s off) = "(Data " <> s <> " " <> show off <> ")"
showOperand (PseudoMem s off) = "(PseudoMem " <> s <> " " <> show off <> ")"
showOperand (Indexed ix) = "(Indexed " <> show ix.baseReg <> " " <> show ix.index <> " " <> show ix.scale <> ")"

showInstruction :: AsmInstruction -> String
showInstruction = case _ of
  Mov _ src dst -> "Mov " <> showOperand src <> " " <> showOperand dst
  Movsx sx -> "Movsx " <> showOperand sx.src <> " " <> showOperand sx.dst
  MovZeroExtend zx -> "MovZeroExtend " <> showOperand zx.src <> " " <> showOperand zx.dst
  Lea src dst -> "Lea " <> showOperand src <> " " <> showOperand dst
  Cvttsd2si _ src dst -> "Cvttsd2si " <> showOperand src <> " " <> showOperand dst
  Cvtsi2sd _ src dst -> "Cvtsi2sd " <> showOperand src <> " " <> showOperand dst
  Unary _ _ op -> "Unary " <> showOperand op
  Binary b -> "Binary " <> showOperand b.src <> " " <> showOperand b.dst
  Cmp _ v1 v2 -> "Cmp " <> showOperand v1 <> " " <> showOperand v2
  Idiv _ op -> "Idiv " <> showOperand op
  Div _ op -> "Div " <> showOperand op
  Cdq _ -> "Cdq"
  Jmp target -> "Jmp " <> target
  JmpCC _ target -> "JmpCC " <> target
  SetCC _ op -> "SetCC " <> showOperand op
  Label l -> "Label " <> l
  Push op -> "Push " <> showOperand op
  Pop r -> "Pop " <> show r
  Call f -> "Call " <> f
  Ret -> "Ret"

-- The Allocator: closure returning `allocate`
makeAllocator :: RegTypeOps -> AllocateFn
makeAllocator r =
  let
    regsToOperands :: List AsmReg -> List AsmOperand
    regsToOperands regs = map (\reg -> Reg reg) regs

    allHardregs :: OperandSet
    allHardregs = Set.fromFoldable (regsToOperands r.all_hardregs)

    callerSavedRegs :: OperandSet
    callerSavedRegs = Set.fromFoldable (regsToOperands r.caller_saved_regs)

    k :: Int
    k = Set.size allHardregs

    _showNodeId :: AsmOperand -> Either CompilerError String
    _showNodeId nd =
      let s = case nd of
                Reg reg -> Right (show reg)
                Pseudo p -> Right p
                _ -> Left (InternalError "malformed interference graph")
      in map (SCU.fromCharArray <<< map (\c -> if c == '.' then '_' else c) <<< SCU.toCharArray) s

    -- Helper to compute used/written registers from an instruction
    regsUsedAndWrittenWith :: (String -> List AsmReg) -> AsmInstruction -> Tuple OperandSet OperandSet
    regsUsedAndWrittenWith paramRegsForCall i =
      let pair = case i of
            Mov _ src dst -> Tuple (src : Nil) (dst : Nil)
            MovZeroExtend zx -> Tuple (zx.src : Nil) (zx.dst : Nil)
            Movsx sx -> Tuple (sx.src : Nil) (sx.dst : Nil)
            Cvtsi2sd _ src dst -> Tuple (src : Nil) (dst : Nil)
            Cvttsd2si _ src dst -> Tuple (src : Nil) (dst : Nil)
            Binary b -> Tuple (b.src : b.dst : Nil) (b.dst : Nil)
            Unary _ _ op -> Tuple (op : Nil) (op : Nil)
            Cmp _ v1 v2 -> Tuple (v1 : v2 : Nil) Nil
            SetCC _ op -> Tuple Nil (op : Nil)
            Push v -> Tuple (v : Nil) Nil
            Idiv _ op -> Tuple (op : Reg AX : Reg DX : Nil) (Reg AX : Reg DX : Nil)
            Div _ op -> Tuple (op : Reg AX : Reg DX : Nil) (Reg AX : Reg DX : Nil)
            Cdq _ -> Tuple (Reg AX : Nil) (Reg DX : Nil)
            Call f ->
              let used =
                    map (\reg -> Reg reg)
                      (filter (\reg -> List.elem reg r.all_hardregs) (paramRegsForCall f))
              in Tuple used (Set.toUnfoldable callerSavedRegs :: List AsmOperand)
            Lea src dst -> Tuple (src : Nil) (dst : Nil)
            Jmp _ -> Tuple Nil Nil
            JmpCC _ _ -> Tuple Nil Nil
            Label _ -> Tuple Nil Nil
            Ret -> Tuple Nil Nil
            Pop _ -> Tuple Nil Nil
          (Tuple opsUsed opsWritten) = pair
          regsUsedToRead opr = case opr of
            Pseudo _ -> opr : Nil
            Reg _ -> opr : Nil
            Memory reg _ -> Reg reg : Nil
            Indexed x -> Reg x.baseReg : Reg x.index : Nil
            Imm _ -> Nil
            Data _ _ -> Nil
            PseudoMem _ _ -> Nil
          regsRead1 = concatMap regsUsedToRead opsUsed
          regsUsedToUpdate opr = case opr of
            Pseudo _ -> Tuple Nil (opr : Nil)
            Reg _ -> Tuple Nil (opr : Nil)
            Memory reg _ -> Tuple (Reg reg : Nil) Nil
            Indexed x -> Tuple (Reg x.baseReg : Reg x.index : Nil) Nil
            Imm _ -> Tuple Nil Nil
            Data _ _ -> Tuple Nil Nil
            PseudoMem _ _ -> Tuple Nil Nil
          updatePairs = map regsUsedToUpdate opsWritten
          regsRead2 = concatMap (\(Tuple a _) -> a) updatePairs
          regsWritten = concatMap (\(Tuple _ b) -> b) updatePairs
      in Tuple (Set.fromFoldable (regsRead1 <> regsRead2)) (Set.fromFoldable regsWritten)

    -- Result-returning version: looks up param regs from the symbol table
    regsUsedAndWritten :: AsmSymbolTableMap -> AsmInstruction -> Either CompilerError (Tuple OperandSet OperandSet)
    regsUsedAndWritten asmSymbols i = case i of
      Call f -> do
        paramRegs <- AssemblySymbols.paramRegsUsed f asmSymbols
        Right (regsUsedAndWrittenWith (\_ -> paramRegs) i)
      Pop _ -> Left (InternalError "Internal error")
      _ -> Right (regsUsedAndWrittenWith (\_ -> Nil) i)

    -- Plain-value meet function
    meet :: OperandSet -> Cfg.ControlFlowGraph OperandSet AsmInstruction -> Cfg.BasicBlock OperandSet AsmInstruction -> OperandSet
    meet returnRegs cfg block =
      let getBlockValueDirect n =
            case List.find (\(Tuple kk _) -> kk == n) cfg.basicBlocks of
              Just (Tuple _ blk) -> blk.value
              Nothing -> Set.empty
          updateLive live = case _ of
            Cfg.Entry -> live
            Cfg.Exit -> Set.union live returnRegs
            Cfg.Block n -> Set.union live (getBlockValueDirect n)
      in List.foldl updateLive Set.empty block.succs

    -- Plain-value transfer function
    transfer :: Map String (List AsmReg) -> Cfg.BasicBlock OperandSet AsmInstruction -> OperandSet -> Cfg.BasicBlock OperandSet AsmInstruction
    transfer paramRegsMap block endLiveRegs =
      let paramRegsForCall f =
            case Map.lookup f paramRegsMap of
              Just regs -> regs
              Nothing -> Nil
          processInstr currentLiveRegs (Tuple _ i) =
            let annotatedInstr = Tuple currentLiveRegs i
                pair = regsUsedAndWrittenWith paramRegsForCall i
                (Tuple regsUsed regsWritten) = pair
                withoutKilled = Set.difference currentLiveRegs regsWritten
                newLiveRegs = Set.union withoutKilled regsUsed
            in Tuple newLiveRegs annotatedInstr
          (Tuple incomingLiveRegs annotatedReversedInstructions) =
            foldLeftMap processInstr endLiveRegs (List.reverse block.instructions)
      in block
          { instructions = List.reverse annotatedReversedInstructions
          , value = incomingLiveRegs
          }

    -- Collect all function names referenced by Call instructions
    collectCalledFunctions :: List AsmInstruction -> List String
    collectCalledFunctions instructions =
      List.nub (mapMaybe (\i -> case i of
        Call f -> Just f
        _ -> Nothing) instructions)

    analyzeLiveness :: Boolean -> UniqueIds.Counter -> AsmSymbolTableMap -> String ->
                       Cfg.ControlFlowGraph Unit AsmInstruction -> List AsmInstruction ->
                       Either CompilerError (Tuple UniqueIds.Counter (Tuple (Cfg.ControlFlowGraph OperandSet AsmInstruction) (List (Tuple String String))))
    analyzeLiveness debug counter asmSymbols fn_name cfg instructions = do
      allReturnRegsList <- AssemblySymbols.returnRegsUsed fn_name asmSymbols
      let allReturnRegs = Set.fromFoldable (regsToOperands allReturnRegsList)
          returnRegs = Set.intersection allHardregs allReturnRegs
      let calledFunctions = collectCalledFunctions instructions
      paramRegsPairs <- resultTraverse (\f -> do
        regs <- AssemblySymbols.paramRegsUsed f asmSymbols
        Right (Tuple f regs)) calledFunctions
      let paramRegsMap = Map.fromFoldable paramRegsPairs
      BackwardDataflow.analyze
        debug
        showOperand
        (Set.empty :: OperandSet)
        (\a b -> a == b)
        (Set.toUnfoldable :: OperandSet -> List AsmOperand)
        (meet returnRegs)
        (transfer paramRegsMap)
        Cfg.initializeAnnotation
        Cfg.updateBasicBlock
        (\(blk :: Cfg.BasicBlock OperandSet AsmInstruction) -> blk.value)
        (\(blk :: Cfg.BasicBlock OperandSet AsmInstruction) -> blk.preds)
        (\(cfg' :: Cfg.ControlFlowGraph OperandSet AsmInstruction) -> cfg'.basicBlocks)
        (\(cfg' :: Cfg.ControlFlowGraph OperandSet AsmInstruction) -> cfg'.debugLabel)
        (\lbl (cfg' :: Cfg.ControlFlowGraph OperandSet AsmInstruction) -> cfg' { debugLabel = lbl })
        (Cfg.graphvizToString showInstruction :: (OperandSet -> String) -> UniqueIds.Counter -> Cfg.ControlFlowGraph OperandSet AsmInstruction -> Tuple UniqueIds.Counter (Tuple String String))
        counter
        cfg

    -- The main allocate function
    allocate :: Boolean -> UniqueIds.Counter -> AsmSymbolTableMap -> String -> Set String -> List AsmInstruction ->
                Either CompilerError (Tuple UniqueIds.Counter (Tuple AsmSymbolTableMap (Tuple (List AsmInstruction) (List (Tuple String String)))))
    allocate debug counter asmSymbols fn_name aliased_pseudos instructions = do
      Tuple counter' (Tuple coalescedGraph (Tuple coalescedInstructions dots)) <- coalesceLoop counter instructions Nil
      graphWithSpillCosts <- addSpillCosts coalescedGraph coalescedInstructions
      coloredGraph <- colorGraph graphWithSpillCosts
      Tuple asmSymbols' registerMap <- makeRegisterMap fn_name coloredGraph
      replacedInstrs <- replacePseudoregs coalescedInstructions registerMap
      Right (Tuple counter' (Tuple asmSymbols' (Tuple replacedInstrs dots)))
      where
      mkBaseGraph :: Map AsmOperand AllocNode
      mkBaseGraph =
        let addNode g reg =
              Map.insert reg
                { id: reg
                , neighbors: Set.delete reg allHardregs
                , spillCost: infinity
                , color: Nothing
                , pruned: false
                }
                g
        in List.foldl addNode Map.empty (Set.toUnfoldable allHardregs :: List AsmOperand)

      getPseudoNodes :: List AsmInstruction -> Either CompilerError (List AllocNode)
      getPseudoNodes instrs = do
        let operandsToPseudos = case _ of
              Pseudo p -> Just p
              _ -> Nothing
            getPseudos i = do
              ops <- getOperands i
              Right (mapMaybe operandsToPseudos ops)
        allPseudoLists <- resultTraverse getPseudos instrs
        let allPseudos = List.concat allPseudoLists
        filteredPseudos <- resultTraverse (\p -> do
          isCurrentType <- r.pseudo_is_current_type asmSymbols p
          isStatic <- AssemblySymbols.isStatic p asmSymbols
          let include' = isCurrentType && not (isStatic || Set.member p aliased_pseudos)
          Right (if include' then Just p else Nothing)) allPseudos
        let pseudos = List.sort (List.nub (mapMaybe identity filteredPseudos))
            initializeNode pseudo =
              { id: Pseudo pseudo
              , neighbors: Set.empty
              , spillCost: 0.0
              , color: Nothing
              , pruned: false
              }
        Right (map initializeNode pseudos)

      addPseudoNodes :: Map AsmOperand AllocNode -> List AsmInstruction -> Either CompilerError (Map AsmOperand AllocNode)
      addPseudoNodes graph instrs = do
        nds <- getPseudoNodes instrs
        let addNode g nd = Map.insert nd.id nd g
        Right (List.foldl addNode graph nds)

      addEdge :: Map AsmOperand AllocNode -> AsmOperand -> AsmOperand -> Either CompilerError (Map AsmOperand AllocNode)
      addEdge g nd_id1 nd_id2 = do
        nd1 <- lookupNode g nd_id1
        nd2 <- lookupNode g nd_id2
        Right (Map.insert nd_id1 (nd1 { neighbors = Set.insert nd_id2 nd1.neighbors })
              (Map.insert nd_id2 (nd2 { neighbors = Set.insert nd_id1 nd2.neighbors }) g))

      removeEdge :: Map AsmOperand AllocNode -> AsmOperand -> AsmOperand -> Either CompilerError (Map AsmOperand AllocNode)
      removeEdge g nd_id1 nd_id2 = do
        nd1 <- lookupNode g nd_id1
        nd2 <- lookupNode g nd_id2
        Right (Map.insert nd_id1 (nd1 { neighbors = Set.delete nd_id2 nd1.neighbors })
              (Map.insert nd_id2 (nd2 { neighbors = Set.delete nd_id1 nd2.neighbors }) g))

      degree :: Map AsmOperand AllocNode -> AsmOperand -> Either CompilerError Int
      degree graph nd_id = do
        nd <- lookupNode graph nd_id
        Right (Set.size nd.neighbors)

      areNeighbors :: Map AsmOperand AllocNode -> AsmOperand -> AsmOperand -> Either CompilerError Boolean
      areNeighbors g nd_id1 nd_id2 = do
        nd1 <- lookupNode g nd_id1
        Right (Set.member nd_id2 nd1.neighbors)

      addEdges :: Cfg.ControlFlowGraph OperandSet AsmInstruction -> Map AsmOperand AllocNode -> Either CompilerError (Map AsmOperand AllocNode)
      addEdges livenessCfg interference_graph =
        let handleInstr g (Tuple liveAfterInstr i) = do
              Tuple _ updatedRegs <- regsUsedAndWritten asmSymbols i
              resultFold (\g' l ->
                case i of
                  Mov _ src _ | src == l -> Right g'
                  _ ->
                    resultFold (\g'' u ->
                      if u /= l && Map.member l g'' && Map.member u g''
                      then addEdge g'' l u
                      else Right g''
                    ) g' (Set.toUnfoldable updatedRegs :: List AsmOperand)
              ) g (Set.toUnfoldable liveAfterInstr :: List AsmOperand)
            allInstructions =
              concatMap (\(Tuple _ blk) -> blk.instructions) livenessCfg.basicBlocks
        in resultFold handleInstr interference_graph allInstructions

      buildInterferenceGraph :: UniqueIds.Counter -> List AsmInstruction ->
                                Either CompilerError (Tuple UniqueIds.Counter (Tuple (Map AsmOperand AllocNode) (List (Tuple String String))))
      buildInterferenceGraph counter' instrs = do
        let baseGraph = mkBaseGraph
        graph <- addPseudoNodes baseGraph instrs
        cfg <- AsmCfg.instructionsToCfg fn_name instrs
        Tuple counter'' (Tuple livenessCfg dots) <- analyzeLiveness debug counter' asmSymbols fn_name cfg instrs
        graphWithEdges <- addEdges livenessCfg graph
        Right (Tuple counter'' (Tuple graphWithEdges dots))

      addSpillCosts :: Map AsmOperand AllocNode -> List AsmInstruction -> Either CompilerError (Map AsmOperand AllocNode)
      addSpillCosts graph instrs = do
        let incrCount counts pseudo =
              Map.alter (case _ of
                Nothing -> Just 1
                Just i -> Just (i + 1)) pseudo counts
        operandLists <- resultTraverse getOperands instrs
        let operands = List.concat operandLists
            getPseudo = case _ of
              Pseudo p -> Just p
              _ -> Nothing
            pseudos = mapMaybe getPseudo operands
            countMap = List.foldl incrCount Map.empty pseudos
            setSpillCost nd = case nd.id of
              Pseudo p ->
                case Map.lookup p countMap of
                  Just c -> Right (nd { spillCost = toNumber c })
                  Nothing -> Left (InternalError ("pseudo not found in count map: " <> p))
              _ -> Right nd
        entries <- resultTraverse (\(Tuple kk v) -> do
          v' <- setSpillCost v
          Right (Tuple kk v')) (Map.toUnfoldable graph :: List (Tuple AsmOperand AllocNode))
        Right (Map.fromFoldable entries)

      georgeTest :: Map AsmOperand AllocNode -> AsmOperand -> AsmOperand -> Either CompilerError Boolean
      georgeTest graph hardreg pseudo = do
        pseudoNd <- lookupNode graph pseudo
        results <- resultTraverse (\neighborId -> do
          isNeighbor <- areNeighbors graph neighborId hardreg
          deg <- degree graph neighborId
          Right (isNeighbor || deg < k)) (Set.toUnfoldable pseudoNd.neighbors :: List AsmOperand)
        Right (List.all identity results)

      briggsTest :: Map AsmOperand AllocNode -> AsmOperand -> AsmOperand -> Either CompilerError Boolean
      briggsTest graph x y = do
        xNd <- lookupNode graph x
        yNd <- lookupNode graph y
        let neighbors = Set.union xNd.neighbors yNd.neighbors
        significantNeighborCount <- resultFold (\cnt neighborId -> do
          deg <- degree graph neighborId
          xNeighbor <- areNeighbors graph x neighborId
          yNeighbor <- areNeighbors graph y neighborId
          let adjustedDeg = if xNeighbor && yNeighbor then deg - 1 else deg
          Right (if adjustedDeg >= k then cnt + 1 else cnt)
          ) 0 (Set.toUnfoldable neighbors :: List AsmOperand)
        Right (significantNeighborCount < k)

      conservativeCoalescable :: Map AsmOperand AllocNode -> AsmOperand -> AsmOperand -> Either CompilerError Boolean
      conservativeCoalescable graph src dst = do
        briggsResult <- briggsTest graph src dst
        if briggsResult then Right true
        else case Tuple src dst of
          Tuple (Reg _) _ -> georgeTest graph src dst
          Tuple _ (Reg _) -> georgeTest graph dst src
          _ -> Right false

      updateGraph :: Map AsmOperand AllocNode -> AsmOperand -> AsmOperand -> Either CompilerError (Map AsmOperand AllocNode)
      updateGraph g to_merge to_keep = do
        mergeNd <- lookupNode g to_merge
        g' <- resultFold (\gg neighborId -> do
          gg' <- addEdge gg neighborId to_keep
          removeEdge gg' neighborId to_merge
          ) g (Set.toUnfoldable mergeNd.neighbors :: List AsmOperand)
        Right (Map.delete to_merge g')

      coalesce :: Map AsmOperand AllocNode -> List AsmInstruction -> Either CompilerError (DisjointSets.DisjointSet AsmOperand)
      coalesce graph instrs =
        let processInstr (Tuple g regMap) i = case i of
              Mov _ src dst ->
                let src' = DisjointSets.find src regMap
                    dst' = DisjointSets.find dst regMap
                in if Map.member src' g && Map.member dst' g && src' /= dst'
                   then do
                     neighbors <- areNeighbors g src' dst'
                     if neighbors then Right (Tuple g regMap)
                     else do
                       coalescable <- conservativeCoalescable g src' dst'
                       if not coalescable then Right (Tuple g regMap)
                       else case src' of
                         Reg _ -> do
                           g' <- updateGraph g dst' src'
                           Right (Tuple g' (DisjointSets.union dst' src' regMap))
                         _ -> do
                           g' <- updateGraph g src' dst'
                           Right (Tuple g' (DisjointSets.union src' dst' regMap))
                   else Right (Tuple g regMap)
              _ -> Right (Tuple g regMap)
        in do
          Tuple _ newRegMap <- resultFold processInstr (Tuple graph DisjointSets.init) instrs
          Right newRegMap

      rewriteCoalesced :: List AsmInstruction -> DisjointSets.DisjointSet AsmOperand -> Either CompilerError (List AsmInstruction)
      rewriteCoalesced instrs coalescedRegs =
        let f op = DisjointSets.find op coalescedRegs
            rewriteInstruction = case _ of
              Mov t src dst ->
                let newSrc = f src
                    newDst = f dst
                in if newSrc == newDst then Right Nothing else Right (Just (Mov t newSrc newDst))
              i -> do
                replaced <- replaceOps f i
                Right (Just replaced)
        in do
          results <- resultTraverse rewriteInstruction instrs
          Right (mapMaybe identity results)

      colorGraph :: Map AsmOperand AllocNode -> Either CompilerError (Map AsmOperand AllocNode)
      colorGraph graph = do
        let remaining =
              filter (\nd -> not nd.pruned)
                (map snd (Map.toUnfoldable graph :: List (Tuple AsmOperand AllocNode)))
        case remaining of
          Nil -> Right graph
          _ -> do
            let notPruned nd_id = do
                  nd <- lookupNode graph nd_id
                  Right (not nd.pruned)
                degreeFn nd = do
                  unprunedResults <- resultTraverse (\nId -> do
                    np <- notPruned nId
                    Right (if np then Just nId else Nothing)
                    ) (Set.toUnfoldable nd.neighbors :: List AsmOperand)
                  let unprunedNeighbors = mapMaybe identity unprunedResults
                  Right (List.length unprunedNeighbors)
                findLowDegree = case _ of
                  Nil -> Right Nothing
                  Cons nd rest -> do
                    deg <- degreeFn nd
                    if deg < k then Right (Just nd)
                    else findLowDegree rest
            nextNode <- do
              lowDeg <- findLowDegree remaining
              case lowDeg of
                Just nd -> Right nd
                Nothing -> do
                  spillMetrics <- resultTraverse (\nd -> do
                    deg <- degreeFn nd
                    let metric = nd.spillCost / toNumber deg
                    Right (Tuple nd metric)
                    ) remaining
                  let cmp (Tuple _ m1) (Tuple _ m2) = compare m1 m2
                  case ListUtil.tryMin cmp spillMetrics of
                    Just (Tuple spilled _) -> Right spilled
                    Nothing -> Left (InternalError "no remaining nodes to spill")
            let prunedGraph =
                  Map.alter (case _ of
                    Just nd -> Just (nd { pruned = true })
                    Nothing -> Nothing) nextNode.id graph
            partlyColored <- colorGraph prunedGraph
            let allColors = List.range 0 (k - 1)
                removeNeighborColor neighborId remainingColors = do
                  neighborNd <- lookupNode partlyColored neighborId
                  case neighborNd.color of
                    Just c -> Right (filter (\col -> col /= c) remainingColors)
                    Nothing -> Right remainingColors
            availableColors <- resultFold (\acc elem -> removeNeighborColor elem acc) allColors (Set.toUnfoldable nextNode.neighbors :: List AsmOperand)
            case availableColors of
              Nil -> Right partlyColored
              _ -> do
                let c = case nextNode.id of
                      Reg reg | not (List.elem reg r.caller_saved_regs) ->
                        case ListUtil.tryMax compare availableColors of
                          Just c' -> c'
                          Nothing -> unsafeHead availableColors
                      _ ->
                        case ListUtil.tryMin compare availableColors of
                          Just c' -> c'
                          Nothing -> unsafeHead availableColors
                Right (Map.alter (case _ of
                  Just nd -> Just (nd { pruned = false, color = Just c })
                  Nothing -> Nothing) nextNode.id partlyColored)

      makeRegisterMap :: String -> Map AsmOperand AllocNode -> Either CompilerError (Tuple AsmSymbolTableMap (Map String AsmReg))
      makeRegisterMap fnName graph = do
        colorsToRegs <- resultFold (\colorMap (Tuple nd_id nd) ->
          case nd_id of
            Reg reg ->
              case nd.color of
                Just c -> Right (Map.insert c reg colorMap)
                Nothing -> Left (InternalError "hardreg node without color")
            _ -> Right colorMap
          ) Map.empty (Map.toUnfoldable graph :: List (Tuple AsmOperand AllocNode))
        Tuple usedCalleeSaved regMap <- resultFold (\(Tuple usedCS rm) (Tuple _ nd) ->
          case nd of
            { id: Pseudo p, color: Just c } ->
              case Map.lookup c colorsToRegs of
                Just hardreg ->
                  let usedCS' =
                        if List.elem hardreg r.caller_saved_regs then usedCS
                        else RegSet.add hardreg usedCS
                  in Right (Tuple usedCS' (Map.insert p hardreg rm))
                Nothing -> Left (InternalError ("color not found in register map: " <> show c))
            _ -> Right (Tuple usedCS rm)
          ) (Tuple RegSet.empty Map.empty) (Map.toUnfoldable graph :: List (Tuple AsmOperand AllocNode))
        asmSymbols' <- AssemblySymbols.addCalleeSavedRegsUsed fnName usedCalleeSaved asmSymbols
        Right (Tuple asmSymbols' regMap)

      replacePseudoregs :: List AsmInstruction -> Map String AsmReg -> Either CompilerError (List AsmInstruction)
      replacePseudoregs instrs regMap =
        let f = case _ of
              Pseudo p ->
                case Map.lookup p regMap of
                  Just reg -> Reg reg
                  Nothing -> Pseudo p
              op -> op
        in do
          replaced <- resultTraverse (replaceOps f) instrs
          Right (cleanupMovs replaced)

      coalesceLoop :: UniqueIds.Counter -> List AsmInstruction -> List (Tuple String String) ->
                      Either CompilerError (Tuple UniqueIds.Counter (Tuple (Map AsmOperand AllocNode) (Tuple (List AsmInstruction) (List (Tuple String String)))))
      coalesceLoop counter' currentInstructions accDots = do
        Tuple counter'' (Tuple graph dots) <- buildInterferenceGraph counter' currentInstructions
        let accDots' = accDots <> dots
        coalescedRegs <- coalesce graph currentInstructions
        if DisjointSets.isEmpty coalescedRegs
          then Right (Tuple counter'' (Tuple graph (Tuple currentInstructions accDots')))
          else do
            newInstructions <- rewriteCoalesced currentInstructions coalescedRegs
            coalesceLoop counter'' newInstructions accDots'

  in allocate

-- unsafeHead: should only be called on non-empty lists (guaranteed by caller)
unsafeHead :: forall a. List a -> a
unsafeHead (Cons x _) = x
unsafeHead Nil = unsafeHead Nil -- unreachable

-- Concrete allocators
gpAllocate :: AllocateFn
gpAllocate = makeAllocator
  { suffix: "gp"
  , all_hardregs: AX : BX : CX : DX : DI : SI : R8 : R9 : R12 : R13 : R14 : R15 : Nil
  , caller_saved_regs: AX : CX : DX : DI : SI : R8 : R9 : Nil
  , pseudo_is_current_type: \ast p -> do
      t <- AssemblySymbols.getType p ast
      Right (t /= AsmDouble)
  }

xmmAllocate :: AllocateFn
xmmAllocate = makeAllocator
  { suffix: "xmm"
  , all_hardregs: XMM0 : XMM1 : XMM2 : XMM3 : XMM4 : XMM5 : XMM6 : XMM7 : XMM8 : XMM9 : XMM10 : XMM11 : XMM12 : XMM13 : Nil
  , caller_saved_regs: XMM0 : XMM1 : XMM2 : XMM3 : XMM4 : XMM5 : XMM6 : XMM7 : XMM8 : XMM9 : XMM10 : XMM11 : XMM12 : XMM13 : Nil
  , pseudo_is_current_type: \ast p -> do
      t <- AssemblySymbols.getType p ast
      Right (t == AsmDouble)
  }

-- Entry point: process each Function, running GP then XMM allocation
allocateRegisters :: Boolean -> UniqueIds.Counter -> AsmSymbolTableMap -> Set String -> AsmProgram ->
                     Either CompilerError (Tuple UniqueIds.Counter (Tuple AsmSymbolTableMap (Tuple AsmProgram (List (Tuple String String)))))
allocateRegisters debug counter asmSymbols aliased_pseudos (Program tls) =
  let allocateRegsForFun counter' asmSymbols' fnName instrs = do
        Tuple counter'' (Tuple asmSymbols'' (Tuple instrs' dots1)) <- gpAllocate debug counter' asmSymbols' fnName aliased_pseudos instrs
        Tuple counter''' (Tuple asmSymbols''' (Tuple instrs'' dots2)) <- xmmAllocate debug counter'' asmSymbols'' fnName aliased_pseudos instrs'
        Right (Tuple counter''' (Tuple asmSymbols''' (Tuple instrs'' (dots1 <> dots2))))
      allocInTl counter' asmSymbols' = case _ of
        Function f -> do
          Tuple counter'' (Tuple asmSymbols'' (Tuple instrs dots)) <- allocateRegsForFun counter' asmSymbols' f.name f.instructions
          Right (Tuple counter'' (Tuple asmSymbols'' (Tuple (Function (f { instructions = instrs })) dots)))
        tl -> Right (Tuple counter' (Tuple asmSymbols' (Tuple tl Nil)))
  in map (\(Tuple c (Tuple ast (Tuple tls' dots))) -> Tuple c (Tuple ast (Tuple (Program tls') dots)))
    (resultFold (\(Tuple c (Tuple ast (Tuple acc accDots))) tl -> do
      Tuple c' (Tuple ast' (Tuple tl' dots)) <- allocInTl c ast tl
      Right (Tuple c' (Tuple ast' (Tuple (acc <> (tl' : Nil)) (accDots <> dots))))
    ) (Tuple counter (Tuple asmSymbols (Tuple Nil Nil))) tls)
