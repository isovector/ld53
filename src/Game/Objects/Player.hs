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

  let onGround = touchingGround (collision CollisionCheckGround) ore pos

  let vel'0 = 0

  let vel' = updateVel onGround dt vel vel'0

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

  drawn <- drawPlayer -< (dir, pos', False)

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
  where
    ore = OriginRect sz $ sz & _x *~ 0.5

    sz :: Num a => V2 a
    sz = V2 8 16


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



drawPlayer :: SF (Bool, V2 WorldPos, Bool) Renderable
drawPlayer =
  proc (dir, pos, is_totsugeku) -> do
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
      [ ifA is_totsugeku
          $ drawGameTextureOriginRect
              ChickenTexture
              (mkCenterdOriginRect 24 & #orect_offset . _x -~ bool (-10) 10 dir) pos 0
          $ V2 dir False
      , r
      ]



instance (Floating a, Eq a) => VectorSpace (V2 a) a where
  zeroVector = 0
  (*^) = (Game.Common.*^)
  (^+^) = (+)
  dot = SDL.dot

