module Game.GameMessageHandler where

import Control.Lens (at)
import Data.Maybe (fromMaybe)
import Engine.Globals (global_worlds)
import Engine.Types

handleGameMessage :: GameMessage -> GlobalState -> GlobalState
handleGameMessage (ChangeLevel lvl) =
  #gs_currentLevel .~
    fromMaybe
      (error "switched to bad level")
      (global_worlds GameWorld ^. #w_levels . at lvl)

