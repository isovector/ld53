{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Game where

import           Control.Monad (join)
import qualified Data.Map as M
import           Engine.Globals
import           Engine.ObjectRouter
import           Engine.Prelude
import           Game.World (drawLevel)


#ifndef __HLINT__

initialObjs :: GlobalState -> ObjectMap ObjSF
initialObjs gs
  = foldr (uncurry addStaticObject) (ObjectMap (DynamicId 0) mempty gs mempty)
  $ M.toList
  $ foldMap l_defaultObjs $ gs_loaded_levels gs


game :: GlobalState -> SF RawFrameInfo ((Camera, Renderable), Event (Time, GameOverReason))
game gs0 =
  proc rfi -> do
    (cam, objs, to_draw) <-
      renderObjects gs0 (tileToPos $ V2 19 50) (initialObjs gs0)
          -< rfi
    let gs = objm_globalState objs
        levelsz = fmap (fromIntegral . getPixel)
                $ r_size
                $ l_bounds
                $ head
                $ gs_loaded_levels gs

    bgs <- arr $ foldMap drawLevel -< gs_loaded_levels gs
    reset <- edge -< c_full_restart $ controls rfi
    on_die <- fmap eventToMaybe $ onChange -< gs_game_over $ gs_gameState gs
    t <- time -< ()

    returnA -< (, fmap (t, ) $ mergeEvents
                    [ GORReset <$ reset
                    , maybeToEvent $ join on_die
                    ]) $
      ( cam
      , mconcat
          [ drawBackgroundColor (V4 46 90 137 255)
          , drawParallax levelsz Parallax0 3
          , drawParallax levelsz Parallax1 4
          , drawParallax levelsz Parallax2 5
          , bgs
          , to_draw
          ]
        )

initialGlobalState :: WorldName -> GlobalState
initialGlobalState w
  = GlobalState (toList $ w_levels $ global_worlds w)
  $ GameState mempty mempty 0 mempty Nothing

#endif
