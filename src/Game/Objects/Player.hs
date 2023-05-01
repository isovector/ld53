{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}
{-# OPTIONS_GHC -Wno-orphans   #-}

module Game.Objects.Player where

import           Control.Lens ((*~))
import qualified Data.Set as S
import           Engine.Collision
import           Engine.Drawing
import           Game.Common

data StandState = Standing | Ducking
  deriving stock (Eq, Ord, Show, Read, Generic, Enum, Bounded)

data JumpNumber = First | Second
  deriving stock (Eq, Ord, Show, Read, Generic, Enum, Bounded)

standing :: a -> a -> StandState -> a
standing d _ Ducking = d
standing _ s Standing = s

data Side
  = LeftSide
  | RightSide
  deriving stock (Eq, Ord, Show, Read, Generic, Enum, Bounded)

data PlayerState
  = PStateIdle
  | PStateWalk
  | PStateTakeoff
  | PStateJump PlayerState
  | PStateRise JumpNumber
  | PStateRiseStab
  | PStateFall JumpNumber
  | PStateFallSlice
  | PStateStab
  | PStateAirSlide
  | PStateWalkStab
  | PStateStartSlide
  | PStateSlide
  | PStateKnockback Side
  deriving stock (Eq, Ord, Show, Read, Generic)

data StateHandlerInput = StateHandlerInput
  { shi_oi :: ObjectInput
  , shi_new :: Event ()
  , shi_standstate :: StandState
  , shi_on_ground :: Bool
  , shi_dt :: Time
  }
  deriving (Generic)

type StateHandler = SF StateHandlerInput StateHandlerResult

data StateHandlerResult = StateHandlerResult
  { shr_events :: ObjectEvents
  , shr_dsd    :: DrawSpriteDetails PuppetAnim
  , shr_ore    :: OriginRect Double
  , shr_dir    :: Bool -> Bool
  , shr_vel    :: V2 Double -> V2 Double
  }
  deriving (Generic)

mkDsd :: a -> DrawSpriteDetails a
mkDsd a =
  DrawSpriteDetails
      a
      Just
      0
    $ V2 True False

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

facingToDir :: Num a => Side -> a
facingToDir = side (-1) 1

side :: a -> a -> Side -> a
side l _ LeftSide = l
side _ r RightSide = r

knockbackHandler :: Side -> StateHandler
knockbackHandler dir = proc shi -> do
  let dt = deltaTime $ shi_oi shi

  let shr = mkSHR PlayerKnockback duckingOre $ \v ->
        event (applyGravity dt v) (const $ V2 (facingToDir dir * 300) (-100)) $ shi_new shi

  returnA -< shr & #shr_dir .~ const (side True False dir)



applyGravity :: Double -> V2 Double -> V2 Double
applyGravity dt v = v + gravity ^* dt


pattern T :: Bool
pattern T = True

pattern F :: Bool
pattern F = False

pattern JL :: Maybe Side
pattern JL = Just LeftSide

pattern JR :: Maybe Side
pattern JR = Just RightSide

hasItem :: ObjectInput -> PowerupType -> Bool
hasItem oi pu = S.member pu $ gs_inventory $ gs_gameState $fi_global $ oi_frameInfo oi

player :: V2 WorldPos -> [PowerupType] -> Object
player pos0 starting_pus = loopPre (0, PStateIdle) $ proc (oi, (vel, st)) -> do
  on_start <- nowish () -< ()
  let pos = event (os_pos $ oi_state oi) (const $ pos0 - V2 0 10) on_start

  let def =
        (noObjectState pos)
          { os_collision = Just playerOre
          , os_hp = 100
          }
  let os = event (oi_state oi) (const def) on_start

  st_changed <- onChange -< st

  let collision = getCollisionMap $ globalState oi
  let dt = deltaTime oi
  let on_ground = touchingGround 1 (collision CollisionCheckGround) playerOre pos
      on_elevator = touchingGround 1 (collision CollisionOnElevator) playerOre pos

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
  shr_knockbackl <- knockbackHandler LeftSide         -< input
  shr_knockbackr <- knockbackHandler RightSide        -< input

  shr <- pick -< (st,) $ \case
    PStateIdle       -> shr_idle
    PStateWalk       -> shr_walk
    PStateTakeoff    -> shr_takeoff
    PStateJump _     -> shr_jump
    PStateRise _     -> shr_rise
    PStateFall _     -> shr_fall
    PStateStab       -> shr_stab
    PStateWalkStab   -> shr_walkStab
    PStateRiseStab   -> shr_riseStab
    PStateFallSlice  -> shr_fallSlice
    PStateStartSlide -> shr_startSlide
    PStateSlide      -> shr_slide
    PStateAirSlide   -> shr_airslide
    PStateKnockback LeftSide  -> shr_knockbackl
    PStateKnockback RightSide  -> shr_knockbackr

  let xdir = view _x $ c_dir $ controls oi

  let ore = shr_ore shr

  let vel' = shr_vel shr vel
  let dpos = vel' ^* dt
  let desiredPos = pos + coerce dpos

  let pos' = fromMaybe pos $ move collision ore pos dpos

  let vel''
        = (\want have res -> bool 0 res $ abs(want - have) <= epsilon )
            <$> desiredPos
            <*> pos'
            <*> vel'

  -- transition out

  wants_jump   <- fmap isEvent edge -< c_jump   $ controls oi
  wants_slide  <- fmap isEvent edge -< c_slide  $ controls oi
  wants_attack <- fmap isEvent edge -< c_attack $ controls oi
  let wants_walk = xdir /= 0
  let upwards_v = view _y vel'' < 0

  let can_jump   = hasItem oi PowerupJump
  let can_slide  = hasItem oi PowerupSlide
  let has_sword  = hasItem oi PowerupSword
  let can_double = hasItem oi PowerupDoubleJump

  -- update the world
  let facing' = shr_dir shr $ os_facing $ oi_state oi

  (boxes, anim_done_ev, drawn)
    <- mkPuppet
    -< ( shr_dsd shr & #dsd_flips . _x .~ not facing'
                     & #dsd_remap .~ remapSword has_sword
       , pos
       )

  (dmg_oe, took_dmg, on_die, hp') <- damageHandler PlayerTeam -< (oi, shr_ore shr, boxes)

  let incoming_damage_dir = fmap (bool RightSide LeftSide . (> 0) . view _x) took_dmg


  let anim_done = isEvent anim_done_ev
  let st' =
        case (st,                anim_done,
                                    on_ground,
                                       on_elevator,
                                          wants_walk,
                                             wants_jump && can_jump,
                                                can_double,
                                                   wants_attack,
                                                      wants_slide && can_slide,
                                                         upwards_v,
                                                            has_sword,
                                                               incoming_damage_dir) of
              -- do knockback
              (_,                _, _, _, _, _, _, _, _, _, _, JL) -> PStateKnockback RightSide
              (_,                _, _, _, _, _, _, _, _, _, _, JR) -> PStateKnockback LeftSide
              -- fall off edge
              (PStateIdle,       _, F, F, _, _, _, _, _, _, _, _) -> PStateFall First
              (PStateWalk,       _, F, F, _, _, _, _, _, _, _, _) -> PStateFall First
              (PStateStartSlide, _, F, _, _, _, _, _, _, _, _, _) -> PStateAirSlide
              (PStateSlide,      _, F, _, _, _, _, _, _, _, _, _) -> PStateAirSlide
              -- hit the ground
              (PStateFall _,     _, T, _, _, _, _, _, _, F, _, _) -> PStateIdle
              (PStateAirSlide,   _, T, _, _, _, _, _, _, F, _, _) -> PStateSlide
              -- jumping
              (PStateIdle,       _, _, _, _, T, _, _, _, _, _, _) -> PStateTakeoff
              (PStateWalk,       _, _, _, _, T, _, _, _, _, _, _) -> PStateTakeoff
              (PStateRise First, _, _, _, _, T, T, _, _, _, _, _) -> PStateJump (PStateRise Second)
              (PStateFall First, _, _, _, _, T, T, _, _, _, _, _) -> PStateJump (PStateRise Second)
              (PStateSlide,      _, T, _, _, T, _, _, _, _, _, _) -> PStateJump PStateAirSlide
              (PStateStartSlide, _, T, _, _, T, _, _, _, _, _, _) -> PStateJump PStateAirSlide
              -- attacks
              (PStateIdle,       _, _, _, _, _, _, T, _, _, T, _) -> PStateStab
              (PStateWalk,       _, _, _, _, _, _, T, _, _, T, _) -> PStateWalkStab
              (PStateRise _,     _, _, _, _, _, _, T, _, _, T, _) -> PStateFallSlice
              (PStateFall _,     _, _, _, _, _, _, T, _, F, T, _) -> PStateFallSlice
              -- walking
              (PStateIdle,       _, _, _, T, _, _, _, _, _, _, _) -> PStateWalk
              (PStateWalk,       _, _, _, F, _, _, _, _, _, _, _) -> PStateIdle
              -- do slides
              (PStateIdle,       _, _, _, _, _, _, _, T, _, _, _) -> PStateStartSlide
              (PStateWalk,       _, _, _, _, _, _, _, T, _, _, _) -> PStateStartSlide
              -- anims done
              (PStateTakeoff,    T, _, _, _, _, _, _, _, _, _, _) -> PStateJump (PStateRise First)
              (PStateStartSlide, T, _, _, _, _, _, _, _, _, _, _) -> PStateSlide
              (PStateKnockback _,T, _, _, _, _, _, _, _, _, _, _) -> PStateIdle
              (PStateSlide,      T, _, _, _, _, _, _, _, _, _, _) -> PStateIdle
              (PStateStab,       T, _, _, _, _, _, _, _, _, _, _) -> PStateIdle
              (PStateWalkStab,   T, _, _, _, _, _, _, _, _, _, _) -> PStateIdle
              (PStateRiseStab,   T, _, _, _, _, _, _, _, _, _, _) -> PStateFall Second
              (PStateFallSlice,  T, _, _, _, _, _, _, _, _, _, _) -> PStateFall Second
              -- cancel
              (PStateRiseStab,   _, T, _, _, _, _, _, _, _, _, _) -> PStateIdle
              (PStateFallSlice,  _, T, _, _, _, _, _, _, _, _, _) -> PStateIdle
              -- automatic transitions
              (PStateJump goto,  _, _, _, _, _, _, _, _, _, _, _) -> goto
              (PStateRise j,     _, _, _, _, _, _, _, _, F, _, _) -> PStateFall j
              (p,                _, _, _, _, _, _, _, _, _, _, _) -> p

  -- do hits
  let (_hits, hurts) = splitAnimBoxes boxes

  returnA -< (, (vel'', st')) $
    ObjectOutput
        { oo_events =
            dmg_oe
              & #oe_focus .~ on_start
              & #oe_game_message <>~ (fmap AddInventory starting_pus <$ on_start)
              & #oe_game_message <>~ Event [SetPlayerLocation pos']
              & #oe_die <>~ on_die
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

remapSword :: Bool -> Text -> Maybe Text
remapSword True v        = Just v
remapSword False "Sword" = Nothing
remapSword False t       = Just t

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


touchingGround :: WorldPos -> (V2 WorldPos -> Bool) -> OriginRect Double -> V2 WorldPos -> Bool
touchingGround d toHit ore pos =
    or
      $ fmap toHit
      $ cornersX (coerce ore) Positive
      $ pos + touchDist
  where
  touchDist = V2 0 d


gravity :: Num a => V2 a
gravity = V2 0 625

clampAbs :: (Num a, Ord a) => a -> a -> a
clampAbs maxv val =
  if abs val <= maxv
     then val
     else maxv * signum val




