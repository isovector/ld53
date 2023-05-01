module Game.GameMessageHandler where

import qualified Data.Set as S
import           Engine.Types


handleGameMessage :: GameMessage -> GlobalState -> GlobalState
handleGameMessage (AddInventory pu) =
  #gs_gameState . #gs_inventory <>~ S.singleton pu

