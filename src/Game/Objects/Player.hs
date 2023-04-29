{-# OPTIONS_GHC -Wno-orphans #-}

module Game.Objects.Player where

import           Control.Lens ((*~))
import           Control.Monad (void)
import qualified Data.Set as S
import qualified Data.Text as T
import           Engine.Collision
import           Engine.Drawing
import           FRP.Yampa ((*^))
import           Game.Common
import           Game.Objects.Particle (gore)
import           Game.Objects.Unknown (unknown)
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

  let holding_jump = c_jump $ controls oi
  jump_changed <- onChange -< holding_jump
  let wants_to_jump = jump_changed == Event True




  let is_ducking = view _y arrows == 1

  let vel'0 = fmap fromIntegral arrows ^* walkSpeed
            & _y .~ bool 0 (- jumpPower) (wants_to_jump && onGround)


  let vel' = updateVel onGround holding_jump dt vel vel'0

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

  (boxes, drawn) <- drawPlayer -< (dir, pos', ore, False)
  returnA -< (, vel'') $
    ObjectOutput
        { oo_events =
            mempty
              & #oe_focus .~ mconcat
                  [ start
                  ]
              & #oe_spawn .~ Event (fmap spawnMe boxes)
        , oo_state =
            oi_state oi
              & #os_pos .~ pos'
              & #os_collision .~ Just ore
              & #os_tags %~ S.insert IsPlayer
              & #os_facing .~ dir
        , oo_render = drawn
        }

spawnMe :: AnimBox -> Object
spawnMe ab
  = withLifetime 0
  $ unknown (T.pack $ show $ ab_type ab) (coerce $ r_pos $ ab_rect ab)
  $ OriginRect (r_size $ ab_rect ab) 0


walkSpeed, runSpeed, jumpPower :: Double
walkSpeed = 200
runSpeed = 300
jumpPower = 200

antigravity :: Bool -> V2 Double
antigravity holding_jump = - bool 0 (V2 0 300) holding_jump


airDampening :: Double
airDampening = 0.025

updateVel :: Bool -> Bool -> Time -> V2 Double -> V2 Double -> V2 Double
updateVel True holding_jump dt old_v dv =
    (old_v & _x .~ 0) + dv
updateVel False holding_jump dt old_v dv =
  (old_v + (dv & _x *~ airDampening) + (gravity + antigravity holding_jump) ^* dt)
    & _x %~ clampAbs walkSpeed

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


gravity :: Num a => V2 a
gravity = V2 0 625

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



drawPlayer :: SF (Bool, V2 WorldPos, OriginRect Double, Bool) ([AnimBox], Renderable)
drawPlayer =
  proc (dir, pos, ore, is_totsugeku) -> do
    -- We can fully animate the player as a function of the position!
    V2 vx vy <- derivative -< pos
    (boxes, r) <- mkPuppet
        -<  ( DrawSpriteDetails
                (bool BallerDribble BallerRun $ abs vx >= epsilon && abs vy < epsilon)
                0
                (V2 (not dir) False)
            , pos
            )
    returnA -< (boxes,) $
      mconcat
        [ drawOriginRect (V4 0 255 0 255) ore pos
        , r
        ]



instance (Floating a, Eq a) => VectorSpace (V2 a) a where
  zeroVector = 0
  (*^) = (Game.Common.*^)
  (^+^) = (+)
  dot = SDL.dot

