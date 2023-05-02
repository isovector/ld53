module Game.Objects.Dracula where

import Game.Common
import System.Random (mkStdGen)
import Control.Monad (void)
import Game.Objects.Player (pick)

data DraculaAttack
  = NoAttack
  | BulletHell
  | Teleport
  | HomeBall
  | Lightning
  deriving stock (Eq, Ord, Show, Read, Generic, Enum, Bounded)

data DraculaInput = DraculaInput
  { dh_oi :: ObjectInput
  , dh_onstart :: Event ()
  }
  deriving stock (Generic)

type DraculaHandler = SF DraculaInput (Event (), Anim, ObjectState, ObjectEvents)

noAttackHandler :: DraculaHandler
noAttackHandler = proc dh -> do
  let os = oi_state $ dh_oi dh

  t <- time -< ()
  start <- hold 0 -< t <$ dh_onstart dh
  let dur = t - start
  done <- edge -< dur >= 2

  returnA -< (done, DraculaIdle, os, mempty)


buffer = 32

lightningHandler :: DraculaHandler
lightningHandler = proc dh -> do
  let os = oi_state $ dh_oi dh
      Rectangle (P offset) _ = fi_active_level $ frameInfo $ dh_oi dh

  new_x1 <- fmap coerce $ noiseR (buffer, getWorldPos (view _x logicalSize) - buffer) (mkStdGen 1331) -< ()
  new_x2 <- fmap coerce $ noiseR (buffer, getWorldPos (view _x logicalSize) - buffer) (mkStdGen 1332) -< ()
  new_x3 <- fmap coerce $ noiseR (buffer, getWorldPos (view _x logicalSize) - buffer) (mkStdGen 1333) -< ()
  new_x4 <- fmap coerce $ noiseR (buffer, getWorldPos (view _x logicalSize) - buffer) (mkStdGen 1334) -< ()
  new_x5 <- fmap coerce $ noiseR (buffer, getWorldPos (view _x logicalSize) - buffer) (mkStdGen 1335) -< ()

  t <- time -< ()
  start <- hold 0 -< t <$ dh_onstart dh
  let dur = t - start
  done <- edge -< dur >= 2

  returnA -< (done, DraculaActive, os,) $ mempty
    & #oe_spawn .~ ([ lightning offset new_x1
                    , lightning offset new_x2
                    , lightning offset new_x3
                    , lightning offset new_x4
                    , lightning offset new_x5
                    ] <$ dh_onstart dh)

lightning :: V2 WorldPos -> WorldPos -> Object
lightning offset x = let lifetime = 1.5 in withLifetime lifetime $ proc oi -> do
  on_start <- nowish () -< ()
  let pos = offset + V2 x 0
  let def = (noObjectState pos)
  let os = event (oi_state oi) (const def) on_start
      sz = logicalSize & _x .~ 10
      ore = OriginRect sz (V2 5 0)

  t <- time -< ()
  start <- hold 0 -< t <$ on_start
  let dur = t - start

  (oe_events, _, _, _) <- damageHandler 0.1 OtherTeam -< (oi, ore, mkHurtBox pos ore)
  let painful = dur >= lifetime - 0.1

  returnA -<
    ObjectOutput
      { oo_events = bool mempty oe_events painful
      , oo_render =
          mconcat $
            [ drawOriginRect (V4 255 255 255 20) ore pos
              | not painful] <>
            [ drawGameTextureOriginRect LightningTexture ore pos 0 $ pure False
              | painful]
      , oo_state = os
      }


homerHandler :: DraculaHandler
homerHandler = proc dh -> do
  let os = oi_state $ dh_oi dh
      pos = os_pos os

  t <- time -< ()
  start <- hold 0 -< t <$ dh_onstart dh
  let dur = t - start
  done <- edge -< dur >= 3

  returnA -< (done, bool DraculaIdle DraculaActive $ dur >= 2.5, os, mempty & #oe_spawn .~ ([dracHome pos] <$ done))

bulletHellHandler :: DraculaHandler
bulletHellHandler = proc dh -> do
  let os = oi_state $ dh_oi dh
      pos = os_pos os

  t <- time -< ()
  start <- hold 0 -< t <$ dh_onstart dh
  let dur = t - start
  done <- edge -< dur >= 5
  shoot <- onlyOncePer 0.05 -< Event ()

  let theta = dur * 15
      vel = V2 (cos theta) (sin theta) * 150

  returnA -< (done, DraculaActive, os, ) $
    mempty
      & #oe_spawn <>~ ([dracBullet pos $ coerce vel] <$ shoot)


teleportHandler :: DraculaHandler
teleportHandler = proc dh -> do
  let os = oi_state $ dh_oi dh
      Rectangle (P offset) _ = fi_active_level $ frameInfo $ dh_oi dh

  t <- time -< ()
  start <- hold 0 -< t <$ dh_onstart dh
  let dur = t - start
  done <- edge -< dur >= 1


  new_x <- fmap coerce $ noiseR (buffer, getWorldPos (view _x logicalSize) - buffer) (mkStdGen 1337) -<  ()
  new_y <- fmap coerce $ noiseR (buffer, getWorldPos (view _y logicalSize) - buffer) (mkStdGen 1337) -<  ()

  let pos' = event id (const $ const $ offset + V2 new_x new_y) done

  returnA -< (done, bool DraculaIdle DraculaActive $ dur >= 0.8,  os & #os_pos %~ pos', mempty)


dracBullet :: V2 WorldPos -> V2 WorldPos -> Object
dracBullet pos0 vel = withLifetime 3 $ proc oi -> do
  on_start <- nowish () -< ()
  let def = (noObjectState pos0)
  let os = event (oi_state oi) (const def) on_start
      dt = deltaTime oi
      ore = mkCenterdOriginRect 3
      pos = os_pos os

  (oe_events, _, _, _) <- damageHandler 0.1 OtherTeam -< (oi, ore, mkHurtBox pos ore)

  let pos' = pos + vel ^* coerce dt

  returnA -<
    ObjectOutput
      { oo_events = oe_events
      , oo_render = drawOriginRect (V4 255 0 0 255) ore pos
      , oo_state = os & #os_pos .~ pos'
      }

dracHome :: V2 WorldPos -> Object
dracHome pos0 = withLifetime 8 $ proc oi -> do
  on_start <- nowish () -< ()
  let def = (noObjectState pos0)
  let os = event (oi_state oi) (const def) on_start
      dt = deltaTime oi
      ore = mkCenterdOriginRect 12
      pos = os_pos os
      player = gs_player_loc (gs_gameState $ globalState oi) - V2 0 24

  (oe_events, _, _, _) <- damageHandler 0.1 OtherTeam -< (oi, ore, mkHurtBox pos ore)

  let pos' = pos + (normalize (player - pos)  * 100) ^* coerce dt

  returnA -<
    ObjectOutput
      { oo_events = oe_events
      , oo_render = drawGameTextureOriginRect TeleTexture ore pos 0 $ pure False
      , oo_state = os & #os_pos .~ pos'
      }


dracula :: V2 WorldPos -> Object
dracula pos0 = pauseWhenOffscreen $ loopPre NoAttack $ proc (oi, attack) -> do
  on_start <- nowish () -< ()
  let def = (noObjectState pos0) { os_hp = 100 }
  let os = event (oi_state oi) (const def) on_start
      ore = mkGroundOriginRect $ V2 55 80
      pos = os_pos os

  new_attack <- onChange -< attack
  let input = DraculaInput (oi & #oi_state .~ os) $ void new_attack

  atk_none <- noAttackHandler -< input
  atk_bullet <- bulletHellHandler -< input
  atk_teleport <- teleportHandler -< input
  atk_home <- homerHandler -< input
  atk_lightning <- lightningHandler -< input

  (done, anim, os', evs) <- pick -< (attack,) $ \case
     NoAttack -> atk_none
     BulletHell -> atk_bullet
     Teleport -> atk_teleport
     HomeBall -> atk_home
     Lightning -> atk_lightning

  new_state <- fmap toEnum $ noiseR (fromEnum $ succ NoAttack, fromEnum $ maxBound @DraculaAttack) (mkStdGen 1337) -< ()
  let attack' = event attack (const $ bool NoAttack new_state $ attack == NoAttack) done

  (dmg_oe, _, on_die, hp') <- damageHandler 0.5 OtherTeam -< (oi, ore, mkHurtHitBox pos ore)

  d <- mkAnim -< (DrawSpriteDetails anim Just 0 $ pure False, pos)

  returnA -< (, attack') $
    ObjectOutput
      { oo_events =
          (evs <> dmg_oe)
            & #oe_die <>~ on_die
            & #oe_game_message <>~ ([GameOver GORWin] <$ on_die)
            -- & #oe_spawn <>~ ([dracBullet arc flipped $ pos - coerce (orect_size ore / 2)] <$ throw)
      , oo_render = d
      , oo_state =
        os' & #os_hp %~ hp'
      }
