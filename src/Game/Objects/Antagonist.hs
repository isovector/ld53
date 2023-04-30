module Game.Objects.Antagonist where

import Game.Common
import Game.Objects.Player (playerOre)
import System.Random (mkStdGen)
import Game.Objects.Particle (particle)


antagonist :: V2 WorldPos -> Object
antagonist pos = loopPre PlayerIdleSword $ proc (oi, anim) -> do
  let OriginRect sz off = playerOre

  slash <- occasionally (mkStdGen 0) 2 () -< ()

  on_start <- nowish () -< ()
  let def =
        (noObjectState pos)
          { os_collision = Just playerOre
          }
  let os = event (oi_state oi) (const def) on_start

  (boxes, done, d) <- mkPuppet -< (DrawSpriteDetails anim 0 $ V2 True False, pos)
  let dmg_ev = fmap (sum . fmap d_damage) $ checkDamage' OtherTeam boxes $ oi_events oi
  let dmg = event 0 id dmg_ev

  new_anim <- hold PlayerIdleSword -< mergeEvents
    [ PlayerIdleSword <$ done
    , PlayerStab      <$ slash
    ]

  returnA -< (, new_anim) $
    ObjectOutput
      { oo_events = sendDamage OtherTeam boxes
          & #oe_spawn .~ (fmap (pure . dmgIndicator pos) dmg_ev)
      , oo_render = d
      , oo_state = os
          & #os_hp -~ dmg
      }

dmgIndicator :: V2 WorldPos -> Int -> Object
dmgIndicator pos dmg =
  particle pos (V2 0 (-100)) (OriginRect (pure $ fromIntegral $ dmg * 10) 0) (V4 255 0 0 255) 0 3

