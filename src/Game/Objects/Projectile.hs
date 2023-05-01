module Game.Objects.Projectile where

import Game.Common


projectile :: Bool -> V2 WorldPos -> Object
projectile flipped pos0 = withLifetime 1 $ proc oi -> do
  on_start <- nowish () -< ()
  let def = (noObjectState pos0)
  let os = event (oi_state oi) (const def) on_start
      dt = deltaTime oi
      ore = mkCenterdOriginRect 3
      pos = os_pos os

  (oe_events, _, _, _) <- damageHandler OtherTeam -< (oi, ore, mkHurtBox pos ore)

  returnA -<
    ObjectOutput
      { oo_events = oe_events
      , oo_render = drawOriginRect (V4 255 0 0 255) ore pos
      , oo_state =
        os  & #os_pos .~ pos + V2 (bool id negate flipped 500) 0 ^* coerce dt
      }
