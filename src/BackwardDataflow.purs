module BackwardDataflow where

import Prelude

import Cfg (NodeId(..))
import CompilerError (CompilerError(..))
import Data.Either (Either(..))
import Data.List (List(..), (:), reverse)
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import UniqueIds as UniqueIds

debugPrint :: forall var varSet cfg.
  Boolean ->
  String ->
  (var -> String) ->
  (varSet -> List var) ->
  (((varSet -> String) -> UniqueIds.Counter -> cfg -> Tuple UniqueIds.Counter (Tuple String String))) ->
  UniqueIds.Counter ->
  String ->
  (String -> cfg -> cfg) ->
  cfg ->
  Tuple UniqueIds.Counter (Maybe (Tuple String String))
debugPrint debug extra_tag pp_var elements print_graphviz counter debugLabel setDebugLabel cfg =
  if debug then
    let livevarPrinter liveVars =
          joinWithComma (map pp_var (elements liveVars))
        lbl = debugLabel <> "_dse" <> extra_tag
        (Tuple counter' (Tuple label dotContent)) = print_graphviz livevarPrinter counter (setDebugLabel lbl cfg)
    in Tuple counter' (Just (Tuple label dotContent))
  else Tuple counter Nothing

joinWithComma :: List String -> String
joinWithComma Nil = ""
joinWithComma (Cons x Nil) = x
joinWithComma (Cons x rest) = x <> ", " <> joinWithComma rest

analyze :: forall var varSet cfg0 cfg block.
  Boolean ->
  (var -> String) ->
  varSet ->
  (varSet -> varSet -> Boolean) ->
  (varSet -> List var) ->
  (cfg -> block -> varSet) ->
  (block -> varSet -> block) ->
  (cfg0 -> varSet -> cfg) ->
  (Int -> block -> cfg -> cfg) ->
  (block -> varSet) ->
  (block -> List NodeId) ->
  (cfg -> List (Tuple Int block)) ->
  (cfg -> String) ->
  (String -> cfg -> cfg) ->
  ((varSet -> String) -> UniqueIds.Counter -> cfg -> Tuple UniqueIds.Counter (Tuple String String)) ->
  UniqueIds.Counter ->
  cfg0 ->
  Either CompilerError (Tuple UniqueIds.Counter (Tuple cfg (List (Tuple String String))))
analyze debug pp_var empty equal elements
        meet_fn transfer_fn initialize_annotation update_basic_block
        get_value get_preds get_basic_blocks
        getDebugLabel setDebugLabel print_graphviz
        counter cfg =
  let startingCfg = initialize_annotation cfg empty
      processWorklist counter' currentCfg worklist accDots =
        let (Tuple counter'' maybeDot) = debugPrint debug "_in_progress_" pp_var elements print_graphviz
              counter' (getDebugLabel currentCfg) setDebugLabel currentCfg
            accDots' = case maybeDot of
              Just d -> d : accDots
              Nothing -> accDots
        in case worklist of
          Nil -> Right (Tuple counter'' (Tuple currentCfg (reverse accDots')))
          Cons (Tuple blockIdx blk) rest ->
            let oldAnnotation = get_value blk
                liveVarsAtExit = meet_fn currentCfg blk
                block' = transfer_fn blk liveVarsAtExit
                updatedCfg = update_basic_block blockIdx block' currentCfg
            in if equal oldAnnotation (get_value block') then
                processWorklist counter'' updatedCfg rest accDots'
              else
                case addPredecessors (get_preds block') rest (get_basic_blocks updatedCfg) of
                  Left e -> Left e
                  Right newWorklist ->
                    processWorklist counter'' updatedCfg newWorklist accDots'
  in processWorklist counter startingCfg (reverse (get_basic_blocks startingCfg)) Nil

addPredecessors :: forall block.
  List NodeId ->
  List (Tuple Int block) ->
  List (Tuple Int block) ->
  Either CompilerError (List (Tuple Int block))
addPredecessors preds worklist allBlocks =
  List.foldl
    (\acc pred_ -> case acc of
        Left e -> Left e
        Right wklist ->
          case pred_ of
            Entry -> Right wklist
            Exit -> Left (InternalError "malformed CFG")
            Block n ->
              if List.any (\(Tuple k _) -> k == n) wklist then Right wklist
              else
                case List.find (\(Tuple k _) -> k == n) allBlocks of
                  Just entry -> Right (entry : wklist)
                  Nothing -> Left (InternalError "block not found in CFG"))
    (Right worklist) preds
