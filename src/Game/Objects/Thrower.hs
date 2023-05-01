module Game.Objects.Thrower where

import Game.Common
import System.Random (mkStdGen)
import Game.Objects.Slime
import Game.Objects.Projectile (projectile)


thrower :: V2 WorldPos -> OriginRect Double -> Maybe (V2 WorldPos) -> Double -> Object
thrower pos0 ore mgoal cooldown = pauseWhenOffscreen $ proc oi -> do
  on_start <- nowish () -< ()
  let def = (noObjectState pos0) { os_hp = 5 }
  let os = event (oi_state oi) (const def) on_start
      pos = os_pos os

  step <- occasionally (mkStdGen $ hash pos0) 0.1 () -< ()
  throw <- occasionally (mkStdGen $ hash (pos0 ^* coerce cooldown)) cooldown () -< ()
  pos' <- maybe (pure pos0) (paceBetween 2 pos0 . useYOfFirst pos0) mgoal -< (pos, step)

  let V2 player_x _ = gs_player_loc $ gameState oi
  let flipped = player_x < view _x pos'

  (dmg_oe, _, on_die, hp') <- damageHandler OtherTeam -< (oi, ore, mkHurtHitBox pos ore)

  returnA -<
    ObjectOutput
      { oo_events =
          dmg_oe
            & #oe_die <>~ on_die
            & #oe_spawn <>~ ([projectile flipped $ pos - coerce (orect_size ore / 2)] <$ throw)
      , oo_render = drawOriginRect (V4 0 255 0 128) ore pos
      , oo_state =
        os & #os_hp %~ hp'
           & #os_pos .~ pos'
      }
