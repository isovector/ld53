module Game.Objects.Slime where

import Game.Common
import Game.Objects.Player (playerOre)
import System.Random (mkStdGen)
import Data.Hashable (hash)


slime :: V2 WorldPos -> OriginRect Double -> Maybe (V2 WorldPos) -> Object
slime pos0 ore mgoal = proc (oi) -> do
  on_start <- nowish () -< ()
  let def = (noObjectState pos0) { os_hp = 5 }
  let os = event (oi_state oi) (const def) on_start
      pos = os_pos os

  step <- occasionally (mkStdGen $ hash pos0) 0.1 () -< ()
  pos' <- maybe (pure pos0) (paceBetween 2 pos0 . useYOfFirst pos0) mgoal -< (pos, step)

  (dmg_oe, on_die, hp') <- damageHandler OtherTeam -< (oi, ore, mkHurtHitBox pos ore)

  returnA -<
    ObjectOutput
      { oo_events =
          dmg_oe
            & #oe_die <>~ on_die
      , oo_render = drawOriginRect (V4 0 255 0 128) ore pos
      , oo_state =
        os & #os_hp %~ hp'
           & #os_pos .~ pos'
      }

useYOfFirst :: V2 a -> V2 a -> V2 a
useYOfFirst v1 v2 = v2 & _y .~ view _y v1

paceBetween :: Double -> V2 WorldPos -> V2 WorldPos -> SF (V2 WorldPos, Event ()) (V2 WorldPos)
paceBetween (coerce -> dist) p1 p2 = proc (pos, ev) -> do
  at_p1 <- edge -< distance pos p1 <= 5
  at_p2 <- edge -< distance pos p2 <= 5

  goal <- hold p2 -< mergeEvents
    [ p2 <$ at_p1
    , p1 <$ at_p2
    ]

  let dir = (goal - pos) / (pure $ distance goal pos)

  returnA -< event pos (const $ pos + dir ^* dist) ev



