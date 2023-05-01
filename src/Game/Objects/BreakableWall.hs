module Game.Objects.BreakableWall where

import Game.Common
import Game.Objects.Player (playerOre)
import Data.Hashable (hash)
import qualified Data.Set as S

allPurposes :: Set CollisionPurpose
allPurposes = S.fromList [minBound .. maxBound]

breakableWall :: V2 WorldPos -> OriginRect Double -> Object
breakableWall pos0 ore = proc oi -> do
  on_start <- nowish () -< ()
  let def = (noObjectState pos0) { os_hp = 5, os_collision = Just ore }
  let os = event (oi_state oi) (const def) on_start
      pos = os_pos os

  (dmg_oe, on_die, hp') <- damageHandler OtherTeam -< (oi, ore, mkHitBox pos ore)

  let obj_id = oi_self oi

  returnA -<
    ObjectOutput
      { oo_events =
          dmg_oe
            & #oe_game_message
                <>~ ([RegisterDynCollision obj_id allPurposes $ originRectToRect (coerce ore) pos] <$ on_start)
            & #oe_game_message
                <>~ ([UnregisterDynCollision obj_id] <$ on_die)
      , oo_render = drawOriginRect (V4 0 255 0 128) ore pos
      , oo_state =
          os & #os_hp %~ hp'
      }

