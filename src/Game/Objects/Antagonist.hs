module Game.Objects.Antagonist where

import Game.Common
import Game.Objects.Player (playerOre)
import System.Random (mkStdGen)


antagonist :: V2 WorldPos -> Object
antagonist pos = loopPre PlayerIdleSword $ proc (oi, anim) -> do
  slash <- occasionally (mkStdGen 0) 2 () -< ()

  on_start <- nowish () -< ()
  let def =
        (noObjectState pos)
          { os_collision = Just playerOre
          , os_hp = 5
          }
  let os = event (oi_state oi) (const def) on_start

  (boxes, done, d) <- mkPuppet -< (DrawSpriteDetails anim 0 $ V2 False False, pos)
  (dmg_oe, hp') <- damageHandler OtherTeam -< (oi, boxes)

  new_anim <- hold PlayerIdleSword -< mergeEvents
    [ PlayerIdleSword <$ done
    , PlayerStab      <$ slash
    ]

  returnA -< (, new_anim) $
    ObjectOutput
      { oo_events = dmg_oe
      , oo_render = d
      , oo_state = os & #os_hp %~ hp'
      }

