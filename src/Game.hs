{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import qualified Data.Map as M
import qualified Data.Set as S
import           Engine.Globals
import           Engine.ObjectRouter
import           Engine.Prelude
import           Game.World (drawLevel)
import           Numeric (showFFloat)


#ifndef __HLINT__

initialObjs :: GlobalState -> ObjectMap ObjSF
initialObjs gs
  = foldr (uncurry addStaticObject) (ObjectMap (DynamicId 0) mempty gs mempty)
  $ M.toList
  $ l_defaultObjs $ gs_currentLevel gs


game :: GlobalState -> SF RawFrameInfo ((Camera, Renderable), Event ())
game gs0 =
  proc rfi -> do
    (cam, objs, to_draw) <-
      renderObjects gs0 (tileToPos $ V2 19 50) (initialObjs gs0)
          -< rfi
    let gs = objm_globalState objs
        levelsz = fmap (fromIntegral . getPixel)
                $ r_size
                $ l_bounds
                $ gs_currentLevel gs

    bg <- arr $ uncurry drawLevel -< (gs_layerset gs, gs_currentLevel gs)
    t <- localTime -< ()
    reset <- edge -< c_full_restart $ controls rfi

    returnA -< (, reset) $
      ( cam
      , mconcat
          [ drawBackgroundColor (V4 46 90 137 255)
          , drawParallax levelsz Parallax0 3
          , drawParallax levelsz Parallax1 4
          , drawParallax levelsz Parallax2 5
          , bg
          , to_draw
          ]
        )

formatTime :: Time -> String
formatTime t =
  let (tsecs, tmils) = properFraction @_ @Int t
      mins = lpad 2 '0' $ show $ div tsecs 60
      secs = lpad 2 '0' $ show $ mod tsecs 60
   in mins <> (':' : secs <> ('.' : drop 2 (showFFloat (Just 3) tmils "")))

lpad :: Int -> Char -> String -> String
lpad n c s
  | let l = length s
  , l < n = replicate (n - l) c <> s
  | otherwise = s

initialGlobalState :: WorldName -> GlobalState
initialGlobalState w
  = GlobalState
      (w_levels (global_worlds w) M.! "AutoLayer")
      (S.fromList [Layer3])
      (GameState 0 mempty False 0)

#endif

