module Game.GameMessageHandler where

import qualified Data.Set as S
import           Engine.Types
import qualified Data.Map as M


handleGameMessage :: GameMessage -> GlobalState -> GlobalState
handleGameMessage (AddInventory pu) =
  #gs_gameState . #gs_inventory <>~ S.singleton pu
handleGameMessage (RegisterDynCollision oi p r) =
  #gs_gameState . #gs_dyn_col <>~ M.singleton oi (p, r)
handleGameMessage (UnregisterDynCollision oi) =
  #gs_gameState . #gs_dyn_col %~ M.delete oi

