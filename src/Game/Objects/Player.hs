{-# OPTIONS_GHC -Wno-orphans #-}

module Game.Objects.Player where

import           Control.Lens ((*~))
import           Data.List (partition)
import qualified Data.Set as S
import qualified Data.Text as T
import           Engine.Collision
import           Engine.Drawing
import           Game.Common
import           Game.Objects.Unknown (unknown)

data StandState = Standing | Ducking
  deriving stock (Eq, Ord, Show, Read, Generic, Enum, Bounded)

standing :: a -> a -> StandState -> a
standing d _ Ducking = d
standing _ s Standing = s

data PlayerState
  = PStateIdle
  | PStateWalk
  | PStateTakeoff
  | PStateJump
  | PStateFall
  | PStateFallSlice
  | PStateStab
  | PStateStartSlide
  | PStateSlide
  deriving stock (Eq, Ord, Show, Read, Generic, Enum, Bounded)

data StateHandlerInput = StateHandlerInput
  { shi_oi :: ObjectInput
  , shi_new :: Event ()
  , shi_standing :: StandState
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
  let ss = shi_standing shi
  returnA -<
    mkSHR (standing PlayerDucked PlayerIdleSword ss)
          (standing duckingOre playerOre ss)
      $ event (const 0) (const id) $ shi_new shi

stabHandler :: StateHandler
stabHandler = proc shi -> do
  let ss = shi_standing shi
  returnA -<
    mkSHR (standing PlayerDuckStab PlayerStab ss)
          (standing duckingOre playerOre ss)
      $ const 0

walkHandler :: StateHandler
walkHandler = proc shi -> do
  let xdir = view _x $ c_dir $ controls $ shi_oi shi
      facing = xdir > 0
  returnA -<
    StateHandlerResult
        mempty
        (mkDsd PlayerRun)
        playerOre
        (bool id (const facing) $ xdir /= 0)
      $ _x %~ clampAbs walkSpeed . (+ (fromIntegral xdir * walkSpeed))

takeoffHandler :: StateHandler
takeoffHandler = proc shi -> do
  returnA -< mkSHR PlayerTakeoff playerOre id

jumpHandler :: StateHandler
jumpHandler = proc shi -> do
  returnA -<
    mkSHR PlayerJump playerOre $ subtract $ V2 0 jumpPower


fallHandler :: StateHandler
fallHandler = proc shi -> do
  let oi = shi_oi shi
  let holding_jump = c_jump $ controls oi
  let xdir = view _x $ c_dir $ controls $ shi_oi shi
  let airVel = V2 (fromIntegral xdir * airSpeed) 0
  returnA -< mkSHR PlayerFall playerOre $ \v ->
    updateVel False holding_jump (deltaTime oi) v airVel

fallSliceHandler :: StateHandler
fallSliceHandler = proc shi -> do
  let oi = shi_oi shi
  let holding_jump = c_jump $ controls oi
  let xdir = view _x $ c_dir $ controls $ shi_oi shi
  let airVel = V2 (fromIntegral xdir * airSpeed) 0
  returnA -< mkSHR PlayerFallSlice playerOre $ \v ->
    updateVel False holding_jump (deltaTime oi) v airVel

startSlideHandler :: StateHandler
startSlideHandler = proc shi -> do
  returnA -< mkSHR PlayerSlidePrep playerOre $ const $ V2 slideSpeed 0

slideHandler :: StateHandler
slideHandler = proc shi -> do
  returnA -< mkSHR PlayerSlide playerOre $ const $ V2 slideSpeed 0


player :: V2 WorldPos -> Object
player pos0 = loopPre (0, PStateIdle) $ proc (oi, (vel, st)) -> do
  start <- nowish () -< ()
  let pos = event (os_pos $ oi_state oi) (const pos0) start

  st_changed <- onChange -< st

  -- handle current
  let input = StateHandlerInput oi (() <$ st_changed) Standing
  shr_idle    <- idleHandler    -< input
  shr_walk    <- walkHandler    -< input
  shr_takeoff <- takeoffHandler -< input
  shr_jump    <- jumpHandler    -< input
  shr_fall    <- fallHandler    -< input
  shr_fallSlice  <- fallSliceHandler  -< input
  shr_stab    <- stabHandler    -< input
  shr_startSlide   <- startSlideHandler   -< input
  shr_slide   <- slideHandler   -< input

  shr <- pick -< (st,) $ \case
    PStateIdle -> shr_idle
    PStateWalk -> shr_walk
    PStateTakeoff -> shr_takeoff
    PStateJump -> shr_jump
    PStateFall -> shr_fall
    PStateFallSlice -> shr_fallSlice
    PStateStab -> shr_stab
    PStateStartSlide -> shr_startSlide
    PStateSlide -> shr_slide

  let collision = getCollisionMap $ globalState oi
  let dt = deltaTime oi
  let on_ground = touchingGround (collision CollisionCheckGround) playerOre pos
  let xdir = view _x $ c_dir $ controls oi

  let ore = shr_ore shr

  let vel' = shr_vel shr vel
  let dpos = vel' ^* dt
  let desiredPos = pos + coerce dpos
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
        case (st,                anim_done, on_ground, wants_walk, wants_jump, wants_attack, wants_slide, upwards_v) of
              (PStateIdle,       _,         False,     _,          _,          _,            _,           _    ) -> PStateFall
              (PStateWalk,       _,         False,     _,          _,          _,            _,           _    ) -> PStateFall
              (PStateFall,       _,         True,      _,          _,          _,            _,           False) -> PStateIdle
              (PStateFall,       _,         _,         _,          _,          True,         _,           False) -> PStateFallSlice
              (PStateIdle,       _,         _,         True,       _,          _,            _,           _    ) -> PStateWalk
              (PStateWalk,       _,         _,         False,      _,          _,            _,           _    ) -> PStateIdle
              (PStateIdle,       _,         _,         _,          _,          True,         _,           _    ) -> PStateStab
              (PStateWalk,       _,         _,         _,          _,          True,         _,           _    ) -> PStateStab
              (PStateIdle,       _,         _,         _,          _,          _,            True,        _    ) -> PStateStartSlide
              (PStateWalk,       _,         _,         _,          _,          _,            True,        _    ) -> PStateStartSlide
              (PStateStartSlide, True,      _,         _,          _,          _,            _,           _    ) -> PStateSlide
              (PStateTakeoff,    True,      _,         _,          _,          _,            _,           _    ) -> PStateJump
              (PStateJump,       _,         _,         _,          _,          _,            _,           _    ) -> PStateFall
              (PStateStab,       True,      _,         _,          _,          _,            _,           _    ) -> PStateIdle
              (PStateSlide,      True,      _,         _,          _,          _,            _,           _    ) -> PStateIdle
              (PStateIdle,       _,         _,         _,          True,       _,            _,           _    ) -> PStateTakeoff
              (PStateWalk,       _,         _,         _,          True,       _,            _,           _    ) -> PStateTakeoff
              (PStateFallSlice,  _,         True,      _,          _,          _,            _,           _    ) -> PStateIdle
              (PStateFallSlice,  True,      _,         _,          _,          _,            _,           _    ) -> PStateIdle
              (p,                _,         _,         _,          _,          _,            _,           _    ) -> p

  -- do hits
  let (hits, hurts) = partition ((== Hitbox) . ab_type) boxes

  returnA -< (, (vel', st')) $
    ObjectOutput
        { oo_events =
            mempty
              & #oe_focus .~ mconcat
                  [ start
                  ]
              & #oe_broadcast_message .~ Event (fmap (sendDamage PlayerTeam) hits)
        , oo_state =
            oi_state oi
              & #os_pos .~ pos'
              & #os_collision .~ Just ore
              & #os_tags %~ S.insert IsPlayer
              & #os_facing .~ facing'
        , oo_render = mconcat
            [ drawn
            -- , flip foldMap hits $ \(ab_rect -> Rect abpos absz) ->
            --     drawOriginRect (V4 0 255 0 92) (OriginRect absz 0) $ coerce abpos
            , flip foldMap hurts $ \(ab_rect -> Rect abpos absz) ->
                drawOriginRect (V4 255 0 0 92) (OriginRect absz 0) $ coerce abpos
            ]
        }

pick :: SF (a, a -> b) b
pick = arr $ uncurry $ flip ($)


player' :: V2 WorldPos -> Object
player' pos0 = loopPre 0 $ proc (oi, vel) -> do
  -- TODO(sandy): this is a bad pattern; object constructor should take an
  -- initial pos
  start <- nowish () -< ()
  let pos = event (os_pos $ oi_state oi) (const pos0) start

  let collision = getCollisionMap $ globalState oi

  let dt = deltaTime oi

  let onGround = touchingGround (collision CollisionCheckGround) playerOre pos

  let arrows = c_dir $ controls oi

  let is_ducking = view _y arrows == 1
  let dir = not is_ducking

  let holding_jump = c_jump $ controls oi
  jump_changed <- onChange -< holding_jump
  let wants_to_jump = jump_changed == Event True

  let holding_slide = c_slide $ controls oi
  slide_press <- edge -< holding_slide
  let wants_to_start_slide = bool NoEvent slide_press $ onGround

  maybeSlide <- holdFor slideDur -< (bool (-1) 1 dir) * slideSpeed <$ wants_to_start_slide
  let slide = fromMaybe 0 maybeSlide

  let vel'0 = fmap fromIntegral arrows ^* walkSpeed
            & _y .~ bool 0 (- jumpPower) (wants_to_jump && onGround)
            & _x %~ (+ slide)


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

  wants_stab <- edge -< c_attack $ controls oi
  stabbing <- holdFor 0.5 -< const PlayerStab <$ wants_stab

  returnA -< (, vel'') $
    ObjectOutput
        { oo_events =
            mempty
              & #oe_focus .~ mconcat
                  [ start
                  ]
              -- & #oe_spawn .~ Event (fmap spawnMe boxes)
        , oo_state =
            oi_state oi
              & #os_pos .~ pos'
              & #os_collision .~ Just ore
              & #os_tags %~ S.insert IsPlayer
              & #os_facing .~ dir
        , oo_render = mempty
        }

spawnMe :: AnimBox -> Object
spawnMe ab
  = withLifetime 0
  $ unknown (T.pack $ show $ ab_type ab) (coerce $ r_pos $ ab_rect ab)
  $ OriginRect (r_size $ ab_rect ab) 0

walkSpeed, airSpeed, runSpeed, slideSpeed, slideDur, jumpPower :: Double
walkSpeed = 200
airSpeed = 100
runSpeed = 300
slideSpeed = 300
jumpPower = 200
slideDur = 0.5

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




