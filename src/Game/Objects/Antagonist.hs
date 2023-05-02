{-# LANGUAGE OverloadedStrings #-}

module Game.Objects.Antagonist where

import Game.Common
import Game.Objects.Player (playerOre)
import System.Random (mkStdGen)
import Control.Lens ((*~))


antagonist :: V2 WorldPos -> Object
antagonist pos = pauseWhenOffscreen $ loopPre PlayerIdleSword $ proc (oi, anim) -> do
  slash <- occasionally (mkStdGen $ hash pos) 2 () -< ()

  on_start <- nowish () -< ()
  let def = (noObjectState pos) { os_hp = 5 }
  let os = event (oi_state oi) (const def) on_start
      ore = mkGroundOriginRect $ orect_size playerOre * scale

  let V2 player_x _ = gs_player_loc $ gameState oi
  let flipped = player_x < view _x pos

  (boxes, done, d) <- mkPuppet scale -< (DrawSpriteDetails anim antagonistRemap 0 $ V2 flipped False, pos)
  (dmg_oe, _, on_die, hp') <- damageHandler 0.3 OtherTeam -< (oi, playerOre, mkHitBox pos ore <> boxes)

  new_anim <- hold PlayerIdleSword -< mergeEvents
    [ PlayerIdleSword <$ done
    , PlayerStab      <$ slash
    ]

  returnA -< (, new_anim) $
    ObjectOutput
      { oo_events =
          dmg_oe
            & #oe_die <>~ on_die
      , oo_render = d
      , oo_state = os & #os_hp %~ hp'
      }

antagonistRemap :: Text -> Maybe Text
antagonistRemap t       = Just $ "Antagonist" <> t

scale :: Fractional a => a
scale = 1.2

