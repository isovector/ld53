module Game.GameMessageHandler where

import           Control.Lens ((?~))
import qualified Data.Map as M
import qualified Data.Set as S
import           Engine.Types


handleGameMessage :: GameMessage -> GlobalState -> GlobalState
handleGameMessage (AddInventory pu) =
  #gs_gameState . #gs_inventory <>~ S.singleton pu
handleGameMessage (RegisterDynCollision oi p r) =
  #gs_gameState . #gs_dyn_col %~ M.insert oi (p, r)
handleGameMessage (UnregisterDynCollision oi) =
  #gs_gameState . #gs_dyn_col %~ M.delete oi
handleGameMessage (SetPlayerLocation loc) =
  #gs_gameState . #gs_player_loc .~ loc
handleGameMessage (SendDamageSource ds) =
  #gs_gameState . #gs_damage_set %~ (ds :)
handleGameMessage (GameOver r) =
  #gs_gameState . #gs_game_over ?~ r

