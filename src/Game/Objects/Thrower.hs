{-# LANGUAGE OverloadedStrings #-}

module Game.Objects.Thrower where

import Game.Common
import System.Random (mkStdGen)
import Game.Objects.Slime
import Game.Objects.Projectile
import Game.Objects.Player (playerOre)


thrower :: V2 WorldPos -> ProjectileType -> Maybe (V2 WorldPos) -> Double -> Object
thrower pos0 arc mgoal cooldown = pauseWhenOffscreen $ loopPre PlayerIdleSword $ proc (oi, anim) -> do
  step <- occasionally (mkStdGen $ hash pos0) 0.1 () -< ()
  slash <- occasionally (mkStdGen $ hash pos0) cooldown () -< ()

  on_start <- nowish () -< ()
  let def = (noObjectState pos0) { os_hp = 5 }
  let os = event (oi_state oi) (const def) on_start
      ore = mkGroundOriginRect $ orect_size playerOre * scale
      pos = os_pos os

  let V2 player_x _ = gs_player_loc $ gameState oi
  let flipped = player_x < view _x pos

  (boxes, throw, d) <- mkPuppet scale -< (DrawSpriteDetails anim throwerRemap 0 $ V2 flipped False, pos)
  (dmg_oe, _, on_die, hp') <- damageHandler 0.3 OtherTeam -< (oi, playerOre, mkHurtHitBox pos ore <> boxes)

  new_anim <- hold PlayerIdleNoSword -< mergeEvents
    [ PlayerIdleNoSword <$ throw
    , PlayerStab        <$ slash
    ]

  pos' <- maybe (pure pos0) (paceBetween 2 pos0 . useYOfFirst pos0) mgoal -< (pos, step)

  returnA -< (, new_anim) $
    ObjectOutput
      { oo_events =
          dmg_oe
            & #oe_die <>~ on_die
            & #oe_spawn <>~ ([projectile arc flipped $ pos - coerce (orect_size ore / 2)] <$ throw)
      , oo_render = d
      , oo_state =
          os & #os_hp %~ hp'
             & #os_pos .~ pos'
      }

scale :: Fractional a => a
scale = 1.2

throwerRemap :: Text -> Maybe Text
throwerRemap "Sword" = Nothing
throwerRemap t       = Just $ "Thrower" <> t



-- thrower :: V2 WorldPos -> OriginRect Double -> ProjectileType -> Maybe (V2 WorldPos) -> Double -> Object
-- thrower pos0 ore arc mgoal cooldown = pauseWhenOffscreen $ loopPre proc oi -> do
--   on_start <- nowish () -< ()
--   let def = (noObjectState pos0) { os_hp = 5 }
--   let os = event (oi_state oi) (const def) on_start
--       pos = os_pos os

--   step <- occasionally (mkStdGen $ hash pos0) 0.1 () -< ()
--   throw <- occasionally (mkStdGen $ hash (pos0 ^* coerce cooldown)) cooldown () -< ()
--   pos' <- maybe (pure pos0) (paceBetween 2 pos0 . useYOfFirst pos0) mgoal -< (pos, step)

--   let V2 player_x _ = gs_player_loc $ gameState oi
--   let flipped = player_x < view _x pos'

--   (_, done, d) <- mkPuppet 1 -< (DrawSpriteDetails anim Just 0 $ V2 flipped False, pos)
--   (dmg_oe, _, on_die, hp') <- damageHandler 0.3 OtherTeam -< (oi, ore, mkHurtHitBox pos ore)

--   returnA -<
--     ObjectOutput
--       { oo_events =
--           dmg_oe
--             & #oe_die <>~ on_die
--             & #oe_spawn <>~ ([projectile arc flipped $ pos - coerce (orect_size ore / 2)] <$ throw)
--       , oo_render = drawOriginRect (V4 0 255 0 128) ore pos
--       , oo_state =
--         os & #os_hp %~ hp'
--            & #os_pos .~ pos'
--       }
