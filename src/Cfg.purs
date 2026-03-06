module Cfg where

import Prelude

import CompilerError (CompilerError(..))
import Data.Either (Either(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.List (List(..), (:), filter, reverse, concatMap)
import Data.List as List
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)
import ListUtil as ListUtil
import UniqueIds as UniqueIds

data SimpleInstr
  = Label String
  | ConditionalJump String
  | UnconditionalJump String
  | Return
  | Other

derive instance eqSimpleInstr :: Eq SimpleInstr

data NodeId
  = Entry
  | Block Int
  | Exit

derive instance eqNodeId :: Eq NodeId
derive instance ordNodeId :: Ord NodeId

type BasicBlock v instr =
  { id :: NodeId
  , instructions :: List (Tuple v instr)
  , preds :: List NodeId
  , succs :: List NodeId
  , value :: v
  }

type ControlFlowGraph v instr =
  { basicBlocks :: List (Tuple Int (BasicBlock v instr))
  , entrySuccs :: List NodeId
  , exitPreds :: List NodeId
  , debugLabel :: String
  }

findBlock :: forall v i. Int -> List (Tuple Int (BasicBlock v i)) -> Either CompilerError (BasicBlock v i)
findBlock n blocks =
  case List.find (\(Tuple k _) -> k == n) blocks of
    Just (Tuple _ blk) -> Right blk
    Nothing -> Left (InternalError "block not found in CFG")

getSuccs :: forall v i. NodeId -> ControlFlowGraph v i -> Either CompilerError (List NodeId)
getSuccs Entry cfg = Right cfg.entrySuccs
getSuccs (Block n) cfg = map _.succs (findBlock n cfg.basicBlocks)
getSuccs Exit _ = Right Nil

getBlockValue :: forall v i. Int -> ControlFlowGraph v i -> Either CompilerError v
getBlockValue blocknum cfg = map _.value (findBlock blocknum cfg.basicBlocks)

updateBlock :: forall v i. (BasicBlock v i -> BasicBlock v i) -> Int -> ControlFlowGraph v i -> ControlFlowGraph v i
updateBlock f blockNum g =
  let newBlocks = map (\(Tuple i blk) -> if i == blockNum then Tuple i (f blk) else Tuple i blk) g.basicBlocks
  in g { basicBlocks = newBlocks }

updateSuccessors :: forall v i. (List NodeId -> List NodeId) -> NodeId -> ControlFlowGraph v i -> Either CompilerError (ControlFlowGraph v i)
updateSuccessors f Entry g = Right (g { entrySuccs = f g.entrySuccs })
updateSuccessors f (Block n) g = Right (updateBlock (\blk -> blk { succs = f blk.succs }) n g)
updateSuccessors _ Exit _ = Left (InternalError "malformed CFG: updateSuccessors on Exit")

updatePredecessors :: forall v i. (List NodeId -> List NodeId) -> NodeId -> ControlFlowGraph v i -> Either CompilerError (ControlFlowGraph v i)
updatePredecessors _ Entry _ = Left (InternalError "malformed CFG: updatePredecessors on Entry")
updatePredecessors f (Block n) g = Right (updateBlock (\blk -> blk { preds = f blk.preds }) n g)
updatePredecessors f Exit g = Right (g { exitPreds = f g.exitPreds })

addEdge :: forall v i. NodeId -> NodeId -> ControlFlowGraph v i -> Either CompilerError (ControlFlowGraph v i)
addEdge pred_ succ_ g =
  let addId ndId idList =
        if List.elem ndId idList then idList
        else ndId : idList
  in case updateSuccessors (addId succ_) pred_ g of
    Right g' -> updatePredecessors (addId pred_) succ_ g'
    Left e -> Left e

removeEdge :: forall v i. NodeId -> NodeId -> ControlFlowGraph v i -> Either CompilerError (ControlFlowGraph v i)
removeEdge pred_ succ_ g =
  let removeId ndId idList = filter (\i -> i /= ndId) idList
  in case updateSuccessors (removeId succ_) pred_ g of
    Right g' -> updatePredecessors (removeId pred_) succ_ g'
    Left e -> Left e

updateBasicBlock :: forall v i. Int -> BasicBlock v i -> ControlFlowGraph v i -> ControlFlowGraph v i
updateBasicBlock blockIdx newBlock g =
  let newBlocks = map (\(Tuple i _) -> if i == blockIdx then Tuple i newBlock else Tuple i (unsafeGetBlock i g.basicBlocks)) g.basicBlocks
  in g { basicBlocks = newBlocks }
  where
  unsafeGetBlock idx blocks =
    case List.find (\(Tuple k _) -> k == idx) blocks of
      Just (Tuple _ blk) -> blk
      Nothing -> newBlock -- fallback, shouldn't happen

-- constructing the CFG
partitionIntoBasicBlocks :: forall a. (a -> SimpleInstr) -> List a -> List (List a)
partitionIntoBasicBlocks simplify instructions =
  let result = List.foldl f { finished: Nil, current: Nil } instructions
      allBlocks = case result.current of
        Nil -> result.finished
        _ -> (reverse result.current) : result.finished
  in reverse allBlocks
  where
  f { finished, current } i =
    case simplify i of
      Label _ ->
        let finished' = case current of
              Nil -> finished
              _ -> (reverse current) : finished
        in { finished: finished', current: i : Nil }
      ConditionalJump _ -> { finished: (reverse (i : current)) : finished, current: Nil }
      UnconditionalJump _ -> { finished: (reverse (i : current)) : finished, current: Nil }
      Return -> { finished: (reverse (i : current)) : finished, current: Nil }
      Other -> { finished, current: i : current }

addAllEdges :: forall v i. (i -> SimpleInstr) -> ControlFlowGraph v i -> Either CompilerError (ControlFlowGraph v i)
addAllEdges simplify g =
  let -- build label -> block id map
      labelMap = List.foldl buildLabelMap Map.empty g.basicBlocks
      buildLabelMap lblMap (Tuple _ blk) =
        case blk.instructions of
          Cons (Tuple _ firstInstr) _ ->
            case simplify firstInstr of
              Label lbl -> Map.insert lbl blk.id lblMap
              _ -> lblMap
          Nil -> lblMap
      findLabel target =
        case Map.lookup target labelMap of
          Just id_ -> Right id_
          Nothing -> Left (InternalError ("label not found in CFG: " <> target))
      processNode (Tuple id_num block) gResult =
        case gResult of
          Left e -> Left e
          Right g' ->
            let next_block = case ListUtil.tryLast g'.basicBlocks of
                  Just (Tuple lastIdx _) | id_num == lastIdx -> Exit
                  _ -> Block (id_num + 1)
            in case ListUtil.tryLast block.instructions of
              Nothing -> Left (InternalError "empty basic block")
              Just (Tuple _ last_instr) ->
                case simplify last_instr of
                  Return -> addEdge block.id Exit g'
                  UnconditionalJump target ->
                    case findLabel target of
                      Right target_id -> addEdge block.id target_id g'
                      Left e -> Left e
                  ConditionalJump target ->
                    case findLabel target of
                      Right target_id ->
                        case addEdge block.id next_block g' of
                          Right g'' -> addEdge block.id target_id g''
                          Left e -> Left e
                      Left e -> Left e
                  _ -> addEdge block.id next_block g'
  in case addEdge Entry (Block 0) g of
    Left e -> Left e
    Right g' -> List.foldl (\acc b -> processNode b acc) (Right g') g'.basicBlocks

instructionsToCfg :: forall i. (i -> SimpleInstr) -> String -> List i -> Either CompilerError (ControlFlowGraph Unit i)
instructionsToCfg simplify debugLabel instructions =
  let toNode idx instrs =
        Tuple idx
          { id: Block idx
          , instructions: map (\x -> Tuple unit x) instrs
          , preds: Nil
          , succs: Nil
          , value: unit
          }
      blocks = partitionIntoBasicBlocks simplify instructions
      cfg =
        { basicBlocks: mapWithIndex toNode blocks
        , entrySuccs: Nil
        , exitPreds: Nil
        , debugLabel: debugLabel
        }
  in addAllEdges simplify cfg

-- converting back to instructions
cfgToInstructions :: forall v i. ControlFlowGraph v i -> List i
cfgToInstructions g =
  concatMap (\(Tuple _ blk) -> map snd blk.instructions) g.basicBlocks

-- working with annotations
initializeAnnotation :: forall v1 v2 i. ControlFlowGraph v1 i -> v2 -> ControlFlowGraph v2 i
initializeAnnotation cfg dummyVal =
  let initializeInstruction (Tuple _ i) = Tuple dummyVal i
      initializeBlock (Tuple idx b) =
        Tuple idx
          { id: b.id
          , instructions: map initializeInstruction b.instructions
          , preds: b.preds
          , succs: b.succs
          , value: dummyVal
          }
  in { basicBlocks: map initializeBlock cfg.basicBlocks
     , entrySuccs: cfg.entrySuccs
     , exitPreds: cfg.exitPreds
     , debugLabel: cfg.debugLabel
     }

stripAnnotations :: forall v i. ControlFlowGraph v i -> ControlFlowGraph Unit i
stripAnnotations cfg = initializeAnnotation cfg unit

-- debugging: generate Graphviz DOT string from a CFG
graphvizToString :: forall v i. (i -> String) -> (v -> String) -> UniqueIds.Counter -> ControlFlowGraph v i -> Tuple UniqueIds.Counter (Tuple String String)
graphvizToString ppInstr ppVal counter cfg =
  let (Tuple c lbl) = UniqueIds.makeLabel cfg.debugLabel counter
      ppNodeId = case _ of
        Exit -> "exit"
        Entry -> "entry"
        Block n -> "block" <> show n
      ppAnnotatedInstruction (Tuple v i) =
        "<tr><td align=\"left\">" <> ppInstr i <> "</td><td align=\"left\">" <> ppVal v <> "</td></tr>\n"
      ppBlockInstructions blk =
        "<table><tr><td colspan=\"2\"><b>" <> ppNodeId blk.id <> "</b></td></tr>\n"
        <> concatStrings (map ppAnnotatedInstruction blk.instructions)
        <> "<tr><td colspan=\"2\">" <> ppVal blk.value <> "</td></tr></table>"
      ppBlock (Tuple blockLbl b) =
        "block" <> show blockLbl <> "[label=<" <> ppBlockInstructions b <> ">]\n"
      ppEntryEdge nodeId =
        "entry -> " <> ppNodeId nodeId <> "\n"
      ppEdges (Tuple i blk) =
        concatStrings (map (\succ_ -> "block" <> show i <> " -> " <> ppNodeId succ_ <> "\n") blk.succs)
      dot =
        "digraph {\n"
        <> "  labeljust=l\n"
        <> "  node[shape=\"box\"]\n"
        <> "  entry[label=\"ENTRY\"]\n"
        <> "  exit[label=\"EXIT\"]\n"
        <> concatStrings (map ppBlock cfg.basicBlocks)
        <> concatStrings (map ppEntryEdge cfg.entrySuccs)
        <> concatStrings (map ppEdges cfg.basicBlocks)
        <> "}\n"
  in Tuple c (Tuple lbl dot)
  where
  concatStrings :: List String -> String
  concatStrings Nil = ""
  concatStrings (Cons s rest) = s <> concatStrings rest
