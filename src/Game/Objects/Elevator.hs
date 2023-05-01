module Game.Objects.Elevator where

import qualified Data.Set as S
import           Game.Common

ground :: Set CollisionPurpose
ground = S.fromList [CollisionCheckGround, CollisionGround, CollisionOnElevator]

elevator :: V2 WorldPos -> OriginRect Double -> Object
elevator pos0 ore = proc oi -> do
  on_start <- nowish () -< ()
  let def = noObjectState pos0

  t <- time -< ()
  let dy = sin (t / 5) * (- 100)

  let os = event (oi_state oi) (const def) on_start
      pos = pos0 - coerce (V2 0 dy)

  let obj_id = oi_self oi

  returnA -<
    ObjectOutput
      { oo_events =
          mempty
            & #oe_game_message
                <>~ Event [RegisterDynCollision obj_id ground $ originRectToRect (coerce ore) pos]
      , oo_render = drawOriginRect (V4 0 255 0 128) ore pos
      , oo_state =
          os & #os_pos .~ pos
      }

