module Game.Objects.BreakableWall where

import qualified Data.Set as S
import           Game.Common

allPurposes :: Set CollisionPurpose
allPurposes = S.fromList [minBound .. maxBound]

breakableWall :: V2 WorldPos -> OriginRect Double -> Object
breakableWall pos0 ore = pauseWhenOffscreen $ proc oi -> do
  on_start <- nowish () -< ()
  let def = (noObjectState pos0) { os_hp = 5, os_collision = Just ore }
  let os = event (oi_state oi) (const def) on_start
      pos = os_pos os

  (dmg_oe, _, on_die, hp') <- damageHandler 0.3 OtherTeam -< (oi, ore, mkHitBox pos ore)

  let obj_id = oi_self oi

  returnA -<
    ObjectOutput
      { oo_events =
          dmg_oe
            & #oe_game_message
                <>~ ([RegisterDynCollision obj_id allPurposes $ originRectToRect (coerce ore) pos] <$ on_start)
            & #oe_game_message
                <>~ ([UnregisterDynCollision obj_id] <$ on_die)
            & #oe_die <>~ on_die
      , oo_render = drawOriginRect (V4 0 0 0 255) ore pos
      , oo_state =
          os & #os_hp %~ hp'
      }

