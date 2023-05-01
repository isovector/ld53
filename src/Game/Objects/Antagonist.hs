module Game.Objects.Antagonist where

import Game.Common
import Game.Objects.Player (playerOre)
import System.Random (mkStdGen)


antagonist :: V2 WorldPos -> Object
antagonist pos = loopPre PlayerIdleSword $ proc (oi, anim) -> do
  slash <- occasionally (mkStdGen 0) 2 () -< ()

  on_start <- nowish () -< ()
  let def = (noObjectState pos) { os_hp = 5, os_collision = Just playerOre }
  let os = event (oi_state oi) (const def) on_start

  let V2 player_x _ = gs_player_loc $ gameState oi
  let flipped = player_x < view _x pos

  (boxes, done, d) <- mkPuppet -< (DrawSpriteDetails anim Just 0 $ V2 flipped False, pos)
  (dmg_oe, on_die, hp') <- damageHandler OtherTeam -< (oi, playerOre, boxes)

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

