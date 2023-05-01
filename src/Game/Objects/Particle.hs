module Game.Objects.Particle where

import Data.Hashable (hash)
import Data.Maybe (fromMaybe)
import Engine.Collision (move)
import Engine.Common
import Engine.Drawing
import Engine.Types
import Engine.Utils (nowish, noObjectState, mkCenterdOriginRect)
import FRP.Yampa hiding ((*^))
import SDL (quadrance)


particle
    :: V2 WorldPos
    -> V2 Double
    -> SF (V2 WorldPos) Renderable
    -> Color
    -> V2 Double
    -> Time
    -> Object
particle pos0 vel0 sf col grav life = loopPre vel0 $ proc (oi, vel) -> do
  start <- nowish () -< ()
  die <- after life () -< ()

  let pos = event (os_pos $ oi_state oi) (const pos0) start

  let dt = fi_dt $ oi_frameInfo oi

  let pos' = pos + coerce (vel ^* dt)
      vel' = vel + grav ^* dt

  r <- sf -< pos'

  returnA -< (, vel') $ ObjectOutput
    { oo_events = mempty { oe_die = die }
    , oo_render = r
    , oo_state = (noObjectState pos')
    }


physicalParticle
    :: V2 WorldPos
    -> V2 Double
    -> OriginRect Double
    -> Color
    -> V2 Double
    -> Time
    -> Object
physicalParticle pos0 vel0 ore col grav life =
  loopPre vel0 $ proc (oi, vel) -> do
    start <- nowish () -< ()
    die <- after life () -< ()

    let pos = event (os_pos $ oi_state oi) (const pos0) start

    let dt = fi_dt $ oi_frameInfo oi

    let gs = fi_global $ oi_frameInfo oi
        mpos' = move (getCollisionMap gs) (coerce ore) pos (vel ^* dt)
        pos' = fromMaybe pos mpos'

        vel' = maybe (dt *^ vel) (coerce . subtract pos) (coerce mpos') ^* (1 / dt)
              + grav ^* dt


        pos'' =
          case quadrance vel <= 100 of
            True -> pos
            False -> pos'

    let end = mergeEvents
          [ die
          -- , bool noEvent (Event ()) $ quadrance vel' <= 400
          ]

    returnA -< (, vel') $ ObjectOutput
      { oo_events = mempty
          { oe_die = end
          }
      , oo_render = drawOriginRect col (coerce ore) pos''
      , oo_state = (noObjectState pos'')
      }


gore :: V2 WorldPos -> [Object]
gore pos = do
  let n = 128
  i <- [id @Int 0 .. n]
  let seed = hash (pos * fromIntegral i)
      j = fromIntegral i * (2 * pi / fromIntegral n)
      speed = 50 + mod (seed * 17) 100
      dur = 4 + mod (seed * 9) 2
      vel = V2 (cos j) (sin j) * fromIntegral speed
      trans = fromIntegral $ 92 + mod (seed * 13) 127
      sz = fromIntegral (10 + mod (seed * 7) 15) / 10
      r = fromIntegral $ 92 + mod (seed * 4) 48
  pure
    $ physicalParticle (pos - V2 0 8) vel (mkCenterdOriginRect sz) (V4 r 0 0 trans) (V2 0 150)
    $ fromIntegral dur

-- firework :: V2 WorldPos -> [Object]
-- firework pos = do
--   let n = 128
--   i <- [id @Int 0 .. n]
--   let seed = hash pos * hash i
--       j = fromIntegral i * (2 * pi / fromIntegral n)
--       speed = 25 + mod (seed * 17) 75
--       dur = 4 + mod (seed * 9) 6
--       vel = V2 (cos j) (sin j) * fromIntegral speed
--       r = fromIntegral $ 128 * (mod (seed * 31) 3) - 1
--       g = fromIntegral $ 128 * (mod (seed * 11) 3) - 1
--       b = fromIntegral $ 128 * (mod (seed * 13) 3) - 1
--   pure
--     $ particle pos vel (mkCenterdOriginRect 2) (V4 r g b 192) (V2 0 30)
--     $ fromIntegral dur
