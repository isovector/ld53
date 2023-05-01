module Game.Objects.Projectile where

import Game.Common

data ProjectileType
  = Shuriken
  | BallToss
  deriving (Eq, Ord, Show, Read, Generic, Enum, Bounded)

arcToVel :: ProjectileType -> SF ObjectInput (V2 WorldPos)
arcToVel Shuriken = constant $
   V2 500 0
arcToVel BallToss = loopPre (V2 150 (-350)) $ proc (oi, vel) -> do
  let dt = coerce $ deltaTime oi
  let vel' = vel + V2 0 625 ^* dt
  returnA -< (vel', vel')

lifetimeForArc :: ProjectileType -> Time
lifetimeForArc Shuriken = 1
lifetimeForArc BallToss = 2

projectile :: ProjectileType -> Bool -> V2 WorldPos -> Object
projectile arc flipped pos0 = withLifetime (lifetimeForArc arc) $ proc oi -> do
  on_start <- nowish () -< ()
  let def = (noObjectState pos0)
  let os = event (oi_state oi) (const def) on_start
      dt = deltaTime oi
      ore = mkCenterdOriginRect 3
      pos = os_pos os

  (oe_events, _, _, _) <- damageHandler OtherTeam -< (oi, ore, mkHurtBox pos ore)

  dpos <- arcToVel arc -< oi

  let pos' = pos + (V2 (bool id negate flipped) id <*> dpos) ^* coerce dt

  returnA -<
    ObjectOutput
      { oo_events = oe_events
      , oo_render = drawOriginRect (V4 255 0 0 255) ore pos
      , oo_state = os & #os_pos .~ pos'
      }
