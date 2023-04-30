{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans   #-}

module Game.Resources where

import           Control.Lens (_head)
import           Control.Monad ((<=<))
import           Data.Spriter.Skeleton (loadSchema)
import           Data.Spriter.Types (schemaFolder, folderFile, _fileName)
import           Data.Traversable (for)
import           Engine.Resources
import           Engine.Types
import           Engine.Utils (setGroundOrigin)
import           SDL (Texture, textureWidth, textureHeight)
import           SDL.JuicyPixels (loadJuicyTexture)
import           SDL.Video (queryTexture)
import qualified Sound.ALUT as ALUT
import           System.FilePath ((</>), (<.>), takeFileName, takeDirectory)

import {-# SOURCE #-} Engine.Importer (loadWorld)
import qualified Data.Map as M
import qualified Data.IntMap as IM

newtype Char' = Char' { getChar' :: Char }
  deriving (Eq, Ord, Show, Enum)

instance Bounded Char' where
  minBound = Char' $ toEnum 32
  maxBound = Char' $ toEnum 122


frameCounts :: Anim -> Int
frameCounts (Idle _)   = 1
frameCounts (NoAnim _) = 1
frameCounts (Run _)    = 4

frameSound :: Anim -> Int -> Maybe Sound
frameSound _ _ = Nothing

wrapTexture :: Texture -> IO WrappedTexture
wrapTexture t = do
  q <- queryTexture t
  pure $ WrappedTexture
    { getTexture = t
    , wt_size = V2 (textureWidth q) $ textureHeight q
    , wt_sourceRect = Nothing
    , wt_origin = 0
    }

instance IsResource Anim [WrappedTexture] where
  resourceFolder = "anims"
  resourceExt = "png"
  resourceName _ = "unused"
  load an e _ = do
    rpath <- resourceRootPath
    for [0 .. frameCounts an - 1] $ \i -> do
      let fp = rpath </> "anims" </> animName an <> "_" <> show i <.> "png"
      wt <- wrapTexture =<< loadJuicyTexture (e_renderer e) fp
      pure $ setGroundOrigin wt


animName :: Anim -> FilePath
animName (Idle s) = charName s </> "idle"
animName (NoAnim s) = charName s </> "no_anim"
animName (Run s) = charName s </> "run"


charName :: Sprite -> FilePath
charName MainCharacter = "mc"


loadWrappedTexture :: Engine -> FilePath -> IO WrappedTexture
loadWrappedTexture
  = (wrapTexture <=<)
  . loadJuicyTexture
  . e_renderer

instance IsResource Char' Texture where
  load _
      = loadJuicyTexture
      . e_renderer
  resourceFolder = "font"
  resourceExt    = "png"
  resourceName c = "font-" <> pad 3 '0' (show $ fromEnum c)

pad :: Int -> Char -> String -> String
pad n c s =
  let len = length s
   in case len >= n of
        True -> s
        False -> replicate (n - len) c <> s

instance IsResource GameTexture WrappedTexture where
  load _
      = (wrapTexture <=<)
      . loadJuicyTexture
      . e_renderer
  resourceFolder = "textures"
  resourceExt    = "png"
  resourceName NintendoLogo = "nintendo"
  resourceName ChickenTexture = "chicken"
  resourceName Parallax0 = "parallax0"
  resourceName Parallax1 = "parallax1"
  resourceName Parallax2 = "parallax2"
  resourceName ChargeTexture = "charge"
  resourceName TeleTexture = "teleball"
  resourceName AuraTexture = "aura"
  resourceName TrampolineTexture = "trampoline"
  resourceName KeycapTexture = "key_unpressed"
  resourceName CheckpointTexture = "checkpoint"
  resourceName ActiveCheckpointTexture = "checkpoint-active"
  resourceName EggTexture = "coin"
  resourceName ArrowTexture = "green_arrow"

instance IsResource PuppetName WrappedSchema where
  load name e fp = do
    schema <- fmap (either error id) $ loadSchema fp
    let dir = takeDirectory fp
    rpath <- resourceRootPath
    textures <-
      for (schema ^. schemaFolder . _head . folderFile) $ \ff -> do
        let fn = _fileName ff
        loadWrappedTexture e $ dir </> fn
    pure $ WrappedSchema schema $ IM.fromList $ zip [0..] textures
  resourceFolder = "puppets"
  resourceExt    = "scon"
  resourceName BallerPuppet = "baller"
  resourceName ManPuppet = "man/man"

-- instance IsResource Song ALUT.Source where
--   load _ _ fileName = do
--     buf <- ALUT.createBuffer (ALUT.File fileName)
--     src <- ALUT.genObjectName
--     ALUT.loopingMode src ALUT.$= ALUT.Looping
--     ALUT.buffer src ALUT.$= Just buf
--     pure src
--   resourceFolder = "songs"
--   resourceExt    = "wav"
--   resourceName _ = error "no songs"

-- instance IsResource Sound ALUT.Source where
--   load _ _ fileName = do
--     buf <- ALUT.createBuffer (ALUT.File fileName)
--     src <- ALUT.genObjectName
--     ALUT.loopingMode src ALUT.$= ALUT.OneShot
--     ALUT.buffer src ALUT.$= Just buf
--     pure src
--   resourceFolder = "sounds"
--   resourceExt    = "wav"
--   resourceName _ = error "no sounds"

instance IsResource WorldName World where
  load _ = loadWorld
  resourceFolder = "levels"
  resourceExt    = "ldtk"
  resourceName GameWorld = "game"
  resourceName HelpWorld = "help"


loadResources :: Engine -> IO Resources
loadResources engine = do
  rpath <- resourceRootPath

  textures <- loadResource rpath engine
  -- songs    <- loadResource rpath engine
  -- sounds   <- loadResource rpath engine
  worlds   <- loadResource rpath engine
  anims    <- loadResource rpath engine
  glyphs   <- loadResource rpath engine
  puppets  <- loadResource rpath engine

  pure $ Resources
    { r_engine   = engine
    , r_textures = textures
    , r_sounds   = \ _ -> error "omg"
    , r_songs    = \ _ -> error "omg"
    , r_worlds   = worlds
    , r_anims    = anims
    , r_puppets = puppets
    , r_glyphs   = glyphs . Char'
    }

getPuppetAnim :: PuppetAnim -> CannedAnim
getPuppetAnim BallerDribble     = CannedAnim BallerPuppet "baller" "Dribble" 0.25 1500 True
getPuppetAnim BallerRun         = CannedAnim BallerPuppet "baller" "DribbleRun" 0.25 1500 True
getPuppetAnim PlayerIdleNoSword = CannedAnim ManPuppet "man" "TemplateNoSword" 0.35 1 True
getPuppetAnim PlayerIdleSword   = CannedAnim ManPuppet "man" "Idle" 0.35 1 True
getPuppetAnim PlayerGrabSword   = CannedAnim ManPuppet "man" "GrabSword" 0.35 2000 False
getPuppetAnim PlayerStab        = CannedAnim ManPuppet "man" "Stab" 0.35 1000 False
getPuppetAnim PlayerRun         = CannedAnim ManPuppet "man" "Run" 0.35 1000 True
getPuppetAnim PlayerTakeoff     = CannedAnim ManPuppet "man" "Takeoff" 0.35 1000 False
getPuppetAnim PlayerJump        = CannedAnim ManPuppet "man" "Jump" 0.35 1000 True
getPuppetAnim PlayerJumpStab    = CannedAnim ManPuppet "man" "JumpStab" 0.35 1000 False
getPuppetAnim PlayerFall        = CannedAnim ManPuppet "man" "Fall" 0.35 500 True
getPuppetAnim PlayerFallSlice   = CannedAnim ManPuppet "man" "FallSlice" 0.35 500 False
getPuppetAnim PlayerDucking     = CannedAnim ManPuppet "man" "Ducking" 0.35 1000 False
getPuppetAnim PlayerDucked      = CannedAnim ManPuppet "man" "Ducked" 0.35 500 True
getPuppetAnim PlayerUnducking   = CannedAnim ManPuppet "man" "Unducking" 0.35 1000 False
getPuppetAnim PlayerDuckStab    = CannedAnim ManPuppet "man" "DuckStab" 0.35 1000 False
getPuppetAnim PlayerSlidePrep   = CannedAnim ManPuppet "man" "SlidePrep" 0.35 1000 False
getPuppetAnim PlayerSlide       = CannedAnim ManPuppet "man" "Slide" 0.35 1000 False

