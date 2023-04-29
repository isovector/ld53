{-# OPTIONS_GHC -Wno-orphans #-}

module Game.Objects.Player where

import           Control.Lens ((*~))
import           Control.Monad (void)
import qualified Data.Set as S
import           Engine.Collision
import           Engine.Drawing
import           FRP.Yampa ((*^))
import           Game.Common
import           Game.Objects.Particle (gore)
import qualified SDL.Vect as SDL

player :: V2 WorldPos -> Object
player pos0 = loopPre 0 $ proc (oi, vel) -> do
  -- TODO(sandy): this is a bad pattern; object constructor should take an
  -- initial pos
  start <- nowish () -< ()
  let pos = event (os_pos $ oi_state oi) (const pos0) start

  let collision = getCollisionMap $ globalState oi

  let dt = deltaTime oi

  let onGround = touchingGround (collision CollisionCheckGround) playerOre pos


  let arrows = c_dir $ controls oi

  let is_ducking = view _y arrows == 1

  let vel'0 = fmap fromIntegral arrows ^* (60 * dt)

  let vel' = updateVel onGround dt vel vel'0

  let ore = bool playerOre duckingOre is_ducking

  let dpos = vel' ^* dt
  let desiredPos = pos + coerce dpos
  let pos' = fromMaybe pos $ move collision (coerce ore) pos $ dpos

  let vel''
        = (\want have res -> bool 0 res $ abs(want - have) <= epsilon )
            <$> desiredPos
            <*> pos'
            <*> vel'

  t <- localTime -< ()

  let dir = True

  drawn <- drawPlayer -< (dir, pos', ore, False)

  returnA -< (, vel'') $
    ObjectOutput
        { oo_events =
            mempty
              & #oe_focus .~ mconcat
                  [ start
                  ]
        , oo_state =
            oi_state oi
              & #os_pos .~ pos'
              & #os_collision .~ Just ore
              & #os_tags %~ S.insert IsPlayer
              & #os_facing .~ dir
        , oo_render = drawn
        }

playerOre :: OriginRect Double
playerOre = OriginRect sz $ sz & _x *~ 0.5

duckingOre :: OriginRect Double
duckingOre = OriginRect ducksz $ ducksz & _x *~ 0.5

sz :: Num a => V2 a
sz = V2 24 60

ducksz :: Num a => V2 a
ducksz = V2 24 30


touchingGround :: (V2 WorldPos -> Bool) -> OriginRect Double -> V2 WorldPos -> Bool
touchingGround toHit ore pos =
    or
      $ fmap toHit
      $ cornersX (coerce ore) Positive
      $ pos + touchDist
  where
  touchDist = V2 0 1


updateVelAir :: Time -> V2 Double -> V2 Double -> V2 Double
updateVelAir dt vel dvel =
    freeVel & _x %~ clampAbs maxXSpeed
  where
    maxXSpeed = 110
    freeVel = vel + ((dvel & _y %~ max 0) + gravity) ^* dt

updateVelGround :: Time -> V2 Double -> V2 Double -> V2 Double
updateVelGround dt vel dvel@(V2 dvx _) =
    V2 (maxXSpeed * signum dvx) air_y
  where
    maxXSpeed = 110
    (V2 _ air_y) = vel + dvel + gravity ^* dt

gravity :: Num a => V2 a
gravity = V2 0 625

updateVel :: Bool -> Time -> V2 Double -> V2 Double -> V2 Double
updateVel True = updateVelGround
updateVel False = updateVelAir

clampAbs :: (Num a, Ord a) => a -> a -> a
clampAbs maxv val =
  if abs val <= maxv
     then val
     else maxv * signum val


respawnTime :: Time
respawnTime = 1


dieAndRespawnHandler :: SF (V2 WorldPos, Event a) (RateLimited ObjectEvents)
dieAndRespawnHandler = proc (pos, on_die) -> do
  rateLimit respawnTime
     (arr $ \(ev, pos) ->
        mempty
          & #oe_spawn .~ (gore pos <$ ev)
          & #oe_play_sound .~ ([] <$ ev)
          & #oe_broadcast_message .~ ([PlayerDeath] <$ ev)
          & #oe_game_message .~ ([AddPlayerDeath] <$ ev)
          & #oe_focus .~ void ev
     ) -< (on_die, pos)



drawPlayer :: SF (Bool, V2 WorldPos, OriginRect Double, Bool) Renderable
drawPlayer =
  proc (dir, pos, ore, is_totsugeku) -> do
    -- We can fully animate the player as a function of the position!
    V2 vx vy <- derivative -< pos
    r <- mkAnim
        -<  ( DrawSpriteDetails
                (bool (Idle MainCharacter) (Run MainCharacter) $ abs vx >= epsilon && abs vy < epsilon && not is_totsugeku)
                0
                (V2 (not dir) False)
            , pos
            )
    returnA -< mconcat
      [ drawOriginRect (V4 0 255 0 255) ore pos
      , r
      ]



instance (Floating a, Eq a) => VectorSpace (V2 a) a where
  zeroVector = 0
  (*^) = (Game.Common.*^)
  (^+^) = (+)
  dot = SDL.dot

