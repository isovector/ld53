{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE PatternSynonyms #-}

module Game.Objects.Player where

import           Control.Lens ((*~))
import qualified Data.Set as S
import           Engine.Collision
import           Engine.Drawing
import           Game.Common

data StandState = Standing | Ducking
  deriving stock (Eq, Ord, Show, Read, Generic, Enum, Bounded)

standing :: a -> a -> StandState -> a
standing d _ Ducking = d
standing _ s Standing = s

data PlayerState
  = PStateIdle
  | PStateWalk
  | PStateTakeoff
  | PStateJump PlayerState
  | PStateRise
  | PStateRiseStab
  | PStateFall
  | PStateFallSlice
  | PStateStab
  | PStateAirSlide
  | PStateWalkStab
  | PStateStartSlide
  | PStateSlide
  deriving stock (Eq, Ord, Show, Read, Generic)

data StateHandlerInput = StateHandlerInput
  { shi_oi :: ObjectInput
  , shi_new :: Event ()
  , shi_standstate :: StandState
  , shi_on_ground :: Bool
  , shi_dt :: Time
  }

type StateHandler = SF StateHandlerInput StateHandlerResult

data StateHandlerResult = StateHandlerResult
  { shr_events :: ObjectEvents
  , shr_dsd    :: DrawSpriteDetails PuppetAnim
  , shr_ore    :: OriginRect Double
  , shr_dir    :: Bool -> Bool
  , shr_vel    :: V2 Double -> V2 Double
  }

mkDsd :: a -> DrawSpriteDetails a
mkDsd a = DrawSpriteDetails a 0 $ V2 True False

mkSHR :: PuppetAnim -> OriginRect Double -> (V2 Double -> V2 Double) -> StateHandlerResult
mkSHR a ore f = StateHandlerResult mempty (mkDsd a) ore id f

idleHandler :: StateHandler
idleHandler = proc shi -> do
  let ss = shi_standstate shi
  returnA -<
    mkSHR (standing PlayerDucked PlayerIdleSword ss)
          (standing duckingOre playerOre ss)
      $ event (const 0) (const id) $ shi_new shi

stabHandler :: StateHandler
stabHandler = proc shi -> do
  let ss = shi_standstate shi
  returnA -<
    mkSHR (standing PlayerDuckStab PlayerStab ss)
          (standing duckingOre playerOre ss)
      $ const 0

walkHandler :: PuppetAnim -> Double -> StateHandler
walkHandler anim mult = proc shi -> do
  let xdir = view _x $ c_dir $ controls $ shi_oi shi
      facing = xdir > 0
      speed = walkSpeed * mult
  returnA -<
    StateHandlerResult
        mempty
        (mkDsd anim)
        playerOre
        (bool id (const facing) $ xdir /= 0)
      $ (_x %~ clampAbs speed . (+ (fromIntegral xdir * speed)))
      . (_y .~ 0)

takeoffHandler :: StateHandler
takeoffHandler = proc _ -> do
  returnA -< mkSHR PlayerTakeoff playerOre id

jumpHandler :: StateHandler
jumpHandler = proc _ -> do
  returnA -<
    mkSHR PlayerJump playerOre $ \v -> v & _y .~ -jumpPower

airControlHandler :: PuppetAnim -> StateHandler
airControlHandler anim = proc shi -> do
  let oi = shi_oi shi
  let holding_jump = c_jump $ controls oi
  let xdir = view _x $ c_dir $ controls $ shi_oi shi
  let airVel = V2 (fromIntegral xdir * airSpeed) 0
  returnA -< mkSHR anim playerOre $ \v ->
    updateVel False holding_jump (deltaTime oi) v airVel

slideHandler :: PuppetAnim -> StateHandler
slideHandler anim = proc shi -> do
  let xspeed = bool negate id (os_facing $ oi_state $ shi_oi shi) slideSpeed
  returnA -< mkSHR anim duckingOre $ \v ->
    bool (applyGravity $ shi_dt shi) id (shi_on_ground shi) $
      v & _x .~ xspeed

airSlideHandler :: StateHandler
airSlideHandler = proc shi -> do
  returnA -< mkSHR PlayerAirSlide duckingOre $
    bool (applyGravity $ shi_dt shi) id (shi_on_ground shi)



applyGravity :: Double -> V2 Double -> V2 Double
applyGravity dt v = v + gravity ^* dt


pattern T :: Bool
pattern T = True

pattern F :: Bool
pattern F = False

player :: V2 WorldPos -> Object
player pos0 = loopPre (0, PStateIdle) $ proc (oi, (vel, st)) -> do
  on_start <- nowish () -< ()
  let pos = event (os_pos $ oi_state oi) (const pos0) on_start

  let def =
        (noObjectState pos)
          { os_collision = Just playerOre
          , os_hp = 5
          }
  let os = event (oi_state oi) (const def) on_start

  st_changed <- onChange -< st

  let collision = getCollisionMap $ globalState oi
  let dt = deltaTime oi
  let on_ground = touchingGround (collision CollisionCheckGround) playerOre pos

  -- handle current
  let input = StateHandlerInput oi (() <$ st_changed) Standing on_ground dt
  shr_idle       <- idleHandler                       -< input
  shr_walk       <- walkHandler PlayerRun 1           -< input
  -- TODO(sandy): don't love this one
  shr_walkStab   <- walkHandler PlayerStab 0.5        -< input
  shr_takeoff    <- takeoffHandler                    -< input
  shr_jump       <- jumpHandler                       -< input
  shr_rise       <- airControlHandler PlayerJump      -< input
  shr_riseStab   <- airControlHandler PlayerJumpStab  -< input
  shr_fall       <- airControlHandler PlayerFall      -< input
  shr_fallSlice  <- airControlHandler PlayerFallSlice -< input
  shr_stab       <- stabHandler                       -< input
  shr_startSlide <- slideHandler PlayerSlidePrep      -< input
  shr_slide      <- slideHandler PlayerSlide          -< input
  shr_airslide   <- airSlideHandler                   -< input

  shr <- pick -< (st,) $ \case
    PStateIdle       -> shr_idle
    PStateWalk       -> shr_walk
    PStateWalkStab   -> shr_walkStab
    PStateTakeoff    -> shr_takeoff
    PStateJump _     -> shr_jump
    PStateRise       -> shr_rise
    PStateRiseStab   -> shr_riseStab
    PStateFall       -> shr_fall
    PStateFallSlice  -> shr_fallSlice
    PStateStab       -> shr_stab
    PStateStartSlide -> shr_startSlide
    PStateAirSlide   -> shr_airslide
    PStateSlide      -> shr_slide

  let xdir = view _x $ c_dir $ controls oi

  let ore = shr_ore shr

  let vel' = shr_vel shr vel
  let dpos = vel' ^* dt
  let _desiredPos = pos + coerce dpos
  let pos' = fromMaybe pos $ move collision ore pos dpos

  -- transition out

  wants_jump   <- fmap isEvent edge -< c_jump   $ controls oi
  wants_slide  <- fmap isEvent edge -< c_slide  $ controls oi
  wants_attack <- fmap isEvent edge -< c_attack $ controls oi
  let wants_walk = xdir /= 0
  let upwards_v = view _y vel < 0

  -- update the world
  let facing' = shr_dir shr $ os_facing $ oi_state oi

  (boxes, anim_done_ev, drawn)
    <- mkPuppet
    -< ( shr_dsd shr & #dsd_flips . _x .~ not facing'
       , pos
       )

  let anim_done = isEvent anim_done_ev
  let st' =
        case (st,                anim_done,
                                    on_ground,
                                       wants_walk,
                                          wants_jump,
                                             wants_attack,
                                                wants_slide,
                                                   upwards_v) of
              -- fall off edge
              (PStateIdle,       _, F, _, _, _, _, _) -> PStateFall
              (PStateWalk,       _, F, _, _, _, _, _) -> PStateFall
              (PStateStartSlide, _, F, _, _, _, _, _) -> PStateAirSlide
              (PStateSlide,      _, F, _, _, _, _, _) -> PStateAirSlide
              -- hit the ground
              (PStateFall,       _, T, _, _, _, _, F) -> PStateIdle
              (PStateAirSlide,   _, T, _, _, _, _, F) -> PStateSlide
              -- jumping
              (PStateIdle,       _, _, _, T, _, _, _) -> PStateTakeoff
              (PStateWalk,       _, _, _, T, _, _, _) -> PStateTakeoff
              (PStateSlide,      _, T, _, T, _, _, _) -> PStateJump PStateAirSlide
              (PStateStartSlide, _, T, _, T, _, _, _) -> PStateJump PStateAirSlide
              -- attacks
              (PStateIdle,       _, _, _, _, T, _, _) -> PStateStab
              (PStateWalk,       _, _, _, _, T, _, _) -> PStateWalkStab
              (PStateRise,       _, _, _, _, T, _, _) -> PStateRiseStab
              (PStateFall,       _, _, _, _, T, _, F) -> PStateFallSlice
              -- walking
              (PStateIdle,       _, _, T, _, _, _, _) -> PStateWalk
              (PStateWalk,       _, _, F, _, _, _, _) -> PStateIdle
              -- do slides
              (PStateIdle,       _, _, _, _, _, T, _) -> PStateStartSlide
              (PStateWalk,       _, _, _, _, _, T, _) -> PStateStartSlide
              -- anims done
              (PStateTakeoff,    T, _, _, _, _, _, _) -> PStateJump PStateRise
              (PStateStartSlide, T, _, _, _, _, _, _) -> PStateSlide
              (PStateSlide,      T, _, _, _, _, _, _) -> PStateIdle
              (PStateStab,       T, _, _, _, _, _, _) -> PStateIdle
              (PStateRiseStab,   T, _, _, _, _, _, _) -> PStateIdle
              (PStateWalkStab,   T, _, _, _, _, _, _) -> PStateIdle
              (PStateFallSlice,  T, _, _, _, _, _, _) -> PStateIdle
              -- cancel
              (PStateRiseStab,   _, T, _, _, _, _, _) -> PStateIdle
              (PStateFallSlice,  _, T, _, _, _, _, _) -> PStateIdle
              -- automatic transitions
              (PStateJump goto,  _, _, _, _, _, _, _) -> goto
              (PStateRise,       _, _, _, _, _, _, F) -> PStateFall
              (p,                _, _, _, _, _, _, _) -> p

  (dmg_oe, hp') <- damageHandler PlayerTeam  -< (oi, shr_ore shr, boxes)

  -- do hits
  let (_hits, hurts) = splitAnimBoxes boxes

  returnA -< (, (vel', st')) $
    ObjectOutput
        { oo_events =
            dmg_oe
              & #oe_focus .~ on_start
        , oo_state =
            os
              & #os_pos .~ pos'
              & #os_collision .~ Just ore
              & #os_tags %~ S.insert IsPlayer
              & #os_facing .~ facing'
              & #os_hp %~ hp'
        , oo_render = mconcat
            [ drawOriginRect (V4 255 255 255 92) ore pos
            , drawn
            -- , flip foldMap hits $ \(ab_rect -> Rect abpos absz) ->
            --     drawOriginRect (V4 0 255 0 92) (OriginRect absz 0) $ coerce abpos
            , flip foldMap hurts $ \(ab_rect -> Rect abpos absz) ->
                drawOriginRect (V4 255 0 0 92) (OriginRect absz 0) $ coerce abpos
            ]
        }

pick :: SF (a, a -> b) b
pick = arr $ uncurry $ flip ($)


walkSpeed, airSpeed, runSpeed, slideSpeed, slideDur, jumpPower :: Double
walkSpeed = 200
airSpeed = 100
runSpeed = 300
slideSpeed = 300
jumpPower = 250
slideDur = 0.5

antigravity :: Bool -> V2 Double
antigravity holding_jump = - bool 0 (V2 0 200) holding_jump


airDampening :: Double
airDampening = 0.025

updateVel :: Bool -> Bool -> Time -> V2 Double -> V2 Double -> V2 Double
updateVel True _ _ old_v dv =
    (old_v & _x .~ 0) + dv
updateVel False holding_jump dt old_v dv =
  (old_v + (dv & _x *~ airDampening) + (gravity + antigravity holding_jump) ^* dt)
    & _x %~ clampAbs walkSpeed

playerOre :: OriginRect Double
playerOre = OriginRect sz $ sz & _x *~ 0.5

duckingOre :: OriginRect Double
duckingOre = OriginRect ducksz $ ducksz & _x *~ 0.5

sz :: Num a => V2 a
sz = V2 16 68

ducksz :: Num a => V2 a
ducksz = V2 16 34


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




