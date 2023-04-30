module Engine.Camera
  ( camera
  , viaCamera
  , getCameraFocus
  ) where

import Engine.Types
import FRP.Yampa
import Engine.Utils (posToTile)


getCameraFocus :: ObjectState -> V2 WorldPos
getCameraFocus os = os_pos os + coerce (os_camera_offset os)

camera
    :: V2 WorldPos
    -> SF (FrameInfo, V2 WorldPos)
          Camera
camera = flip loopPre $ arr $ \((fi, focus), pos) -> do
  let dt = fi_dt fi
      pos' = pos + (focus - pos) ^* coerce dt * cameraSpeed
  let desired =
        case focus == pos || distance focus pos <= cameraDeadzone of
          True -> pos
          False -> pos'
      actual = centerScreen
             $ keepInRect (fmap (fromIntegral . getPixel) . l_bounds . head . gs_loaded_levels $ fi_global fi)
             $ desired
  ( Camera actual
    , desired
    )

keepInRect :: Rect WorldPos -> V2 WorldPos -> V2 WorldPos
keepInRect (Rect (V2 l t) (V2 w h)) (V2 x y) =
  let (V2 hw hh) = halfLogic in
  V2 (clamp (l + hw, l + w - hw) x) (clamp (t + hh, t + h - hh) y)

halfLogic :: V2 WorldPos
halfLogic = logicalSize * 0.5

clamp :: Ord a => (a, a) -> a -> a
clamp (low, high) a = min high (max a low)



cameraSpeed :: Num a => a
cameraSpeed = 3


centerScreen :: V2 WorldPos -> V2 WorldPos
centerScreen focus = -focus + logicalSize / 2


viaCamera :: Camera -> V2 WorldPos -> V2 ScreenPos
viaCamera (Camera cam) world = coerce $ cam + world


cameraDeadzone :: Num a => a
cameraDeadzone = 5

