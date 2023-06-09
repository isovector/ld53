module Game.Splash where

import Game (game, initialGlobalState)
import Game.Common
import SDL (setWindowMode, WindowMode (FullscreenDesktop), get, ($=), rendererScale)
import Engine.Globals (global_resources)
import SDL.Video (windowSize)
import Numeric (showFFloat)


runIntro :: SF RawFrameInfo (Camera, Renderable)
runIntro = runSwont (error "die") $ do
  -- momentary $ (Camera 0, const $ playSong WarmDuckShuffle)
  splashScreen

splashScreen :: Swont RawFrameInfo (Camera, Renderable) ()
splashScreen = do
  swont (liftIntoGame mainMenu) >>= \case
    Start -> do
      end <- swont $ game (initialGlobalState GameWorld)
      case end of
        (_, GORReset) -> splashScreen
        (_, GORDeath) -> do
          swont $ liftIntoGame gameOverScreen
          splashScreen
        (t, GORWin) -> do
          swont $ liftIntoGame $ youWinScreen t
          splashScreen

    Fullscreen -> do
      momentary $ (Camera 0, const $ do
        let e = r_engine global_resources
            w = e_window e
        setWindowMode w FullscreenDesktop
        sz <- get $ windowSize w
        rendererScale (e_renderer e) $= (fmap (fromIntegral . round @Double @Int) $ fmap fromIntegral sz / logicalSize @Double)
                  )
      splashScreen
    Credits -> do
      swont $ liftIntoGame credits
      splashScreen


liftIntoGame :: SF RawFrameInfo (IO (), Event a) -> SF RawFrameInfo ((Camera, Renderable), Event a)
liftIntoGame sf = sf >>> arr (first $ (Camera 0,) . const)



data MenuItem = Start | Fullscreen | Credits
  deriving (Eq, Ord, Show, Enum, Bounded)

prevMenuItem :: MenuItem -> MenuItem
prevMenuItem Start = Credits
prevMenuItem n = pred n

nextMenuItem :: MenuItem -> MenuItem
nextMenuItem Credits = Start
nextMenuItem n = succ n


anyKey :: Controls -> Bool
anyKey c = c_attack c || c_space c || c_jump c

mainMenu :: SF RawFrameInfo (IO (), Event MenuItem)
mainMenu = loopPre Start $ proc (fi, sel) -> do
  ((>>= maybeToEvent) -> y)
      <- onChange -< int2Maybe $ (c_dir $ controls fi) ^. _y

  let f = bool prevMenuItem nextMenuItem <$> y
  let sel' = event sel ($ sel) f

  press <- edge -< anyKey $ fi_controls fi
  idata <- afterEach (snd $ deltaEncode 0.3 $ cycle
                      [ defaultControls
                          { c_dir = V2 1 0 }
                      , defaultControls
                          { c_dir = V2 1 0 }
                      , defaultControls
                          { c_dir = V2 (-1) 0 }
                      , defaultControls
                          { c_space = True
                          , c_dir = V2 1 0
                          }
                      , defaultControls
                          { c_space = True }
                      , defaultControls
                      , defaultControls
                          { c_space = True }
                      , defaultControls
                          { c_space = True
                          , c_dir = V2 1 0
                          }
                      , defaultControls
                          { c_space = True
                          , c_dir = V2 1 0
                          }
                      , defaultControls
                      , defaultControls
                      , defaultControls
                          { c_dir = V2 (-1) 0
                          }
                      , defaultControls
                          { c_jump = True
                          }
                      , defaultControls
                          { c_jump = True
                          }
                      , defaultControls
                          { c_jump = True
                          }
                      , defaultControls
                      , defaultControls
                      , defaultControls
                      , defaultControls
                          { c_reset = True
                          }
                      ]) -< ()
  inputs <- hold defaultControls -< maybeToEvent =<< idata


  ((cam, bggame), _) <- game (initialGlobalState HelpWorld) -< fi & #fi_controls .~ inputs

  returnA -<
    ( ( mconcat
          [ bggame cam
          , drawText 16 (V3 0 192 255) "Deliverance" (V2 50 20) (Camera 0)
          , mconcat
            $ zipWith (drawMenuItem sel) [minBound .. maxBound] [0..]
          ]
      , sel' <$ press
      )
    , sel'
    )

drawMenuItem :: MenuItem -> MenuItem -> Int -> IO ()
drawMenuItem sel mi ix =
    drawText 16 col (show mi)
      ((logicalSize / 2)
          & _x -~ 215
          & _y .~ 180 + fromIntegral ix * 24
      ) (Camera 0)
  where
    col =
      if sel == mi
         then V3 255 255 255
         else V3 120 120 120


gameOverScreen :: SF RawFrameInfo (IO (), Event ())
gameOverScreen = proc rfi -> do
  press <- edge -< anyKey $ fi_controls rfi

  returnA -<
    ( mconcat
        [ drawBackgroundColor (V4 0 0 0 255) (Camera 0)
        , drawText 20 (V3 255 0 0) "GAME OVER" (V2 130 70) (Camera 0)
        ]
    , press
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

youWinScreen :: Time -> SF RawFrameInfo (IO (), Event ())
youWinScreen t = proc rfi -> do
  press <- edge -< anyKey $ fi_controls rfi

  returnA -<
    ( mconcat
        [ drawBackgroundColor (V4 0 0 0 255) (Camera 0)
        , drawText 20 (V3 255 255 255) "THE GALAXY IS AT PEACE" (V2 20 70) (Camera 0)
        , drawText 10 (V3 255 255 255) ("You won in " <> formatTime t) (V2 150 180) (Camera 0)
        ]
    , press
    )


credits :: SF RawFrameInfo (IO (), Event ())
credits = proc rfi -> do
  press <- edge -< anyKey $ fi_controls rfi

  returnA -<
    ( mconcat
        [ drawBackgroundColor (V4 0 0 0 255) (Camera 0)
        , drawText 14 (V3 255 255 255) "Andrew McKnight" (V2 30 70) (Camera 0)
        , drawText 10 (V3 255 255 255) "github.com/amcknight" (V2 30 90) (Camera 0)
        , drawText 14 (V3 255 255 255) "Sandy Maguire" (V2 80 140) (Camera 0)
        , drawText 10 (V3 255 255 255) "github.com/isovector" (V2 80 160) (Camera 0)
        ]
    , press
    )




int2Maybe :: Int -> Maybe Bool
int2Maybe (-1) = Just False
int2Maybe 0 = Nothing
int2Maybe 1 = Just True
int2Maybe _ = Nothing


