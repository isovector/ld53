{-# LANGUAGE CPP             #-}
{-# LANGUAGE RecordWildCards #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

#ifndef __HLINT__

module Engine.Drawing where

import           Control.Lens (at)
import           Control.Monad (guard)
import           Data.Foldable (for_, traverse_)
import           Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import           Data.Maybe (fromJust, mapMaybe)
import           Data.Spriter.Skeleton (ResultBone(..), fmod, animate, isBone)
import           Data.Spriter.Types hiding (AnimationName)
import qualified Data.Text as T
import           Engine.Camera (viaCamera)
import           Engine.FRP
import           Engine.Geometry (rectContains)
import           Engine.Globals (global_resources, global_anims, global_glyphs, global_textures, global_songs, global_sounds, global_puppets)
import           Engine.Types
import           Engine.Utils (originRectToRect)
import           Foreign.C
import           Game.Box (parseBox)
import           Game.Resources (frameSound, frameCounts, getPuppetAnim)
import           SDL hiding (Event)
import qualified Sound.ALUT as ALUT

playSound :: Sound -> IO ()
playSound s = do
  let src = global_sounds s
  ALUT.stop [src]
  ALUT.play [src]


drawOriginRect :: Color -> OriginRect Double -> V2 WorldPos -> Renderable
drawOriginRect c ore = drawFilledRect c . originRectToRect (coerce ore)


drawFilledRect :: Color -> Rectangle WorldPos -> Renderable
drawFilledRect c (Rectangle (P v) sz) cam = do
  let rect' = Rectangle (P $ viaCamera cam v) $ coerce sz
  let renderer = e_renderer $ r_engine global_resources
  rendererDrawColor renderer $= c
  fillRect renderer $ Just $ fmap (round . getScreenPos) rect'

drawBackgroundColor :: Color -> Renderable
drawBackgroundColor c _ = do
  let renderer = e_renderer $ r_engine global_resources
  rendererDrawColor renderer $= c
  fillRect renderer Nothing

drawSpriteStretched
    :: WrappedTexture  -- ^ Texture
    -> V2 WorldPos       -- ^ position
    -> Double          -- ^ rotation in rads
    -> V2 Bool         -- ^ mirroring
    -> V2 Double       -- ^ scaling factor
    -> Renderable
drawSpriteStretched wt pos theta flips stretched cam
  | let wp = viaCamera cam $ pos - coerce (fmap fromIntegral (wt_origin wt) * stretched)
  , rectContains screenRect wp
  = do
      let renderer = e_renderer $ r_engine global_resources
      copyEx
        renderer
        (getTexture wt)
        (wt_sourceRect wt)
        (Just $ fmap round
              $ Rectangle (P $ coerce wp)
              $ fmap fromIntegral (wt_size wt) * stretched)
        (CDouble theta)
        (Just $ fmap round
              $ P
              $ fmap fromIntegral (wt_origin wt) * stretched)
        flips
  | otherwise = mempty

drawGameTextureOriginRect
    :: GameTexture
    -> OriginRect Double
    -> V2 WorldPos     -- ^ position
    -> Double          -- ^ rotation in rads
    -> V2 Bool         -- ^ mirroring
    -> Renderable
drawGameTextureOriginRect = drawTextureOriginRect . global_textures

drawTextureOriginRect
    :: WrappedTexture  -- ^ Texture
    -> OriginRect Double
    -> V2 WorldPos     -- ^ position
    -> Double          -- ^ rotation in rads
    -> V2 Bool         -- ^ mirroring
    -> Renderable
drawTextureOriginRect wt ore pos theta flips cam
  | let wp = viaCamera cam pos
  , rectContains screenRect wp
  = do
      let renderer = e_renderer $ r_engine global_resources
      copyEx
        renderer
        (getTexture wt)
        (wt_sourceRect wt)
        (Just $ fmap round $ originRectToRect ore $ coerce wp)
        (CDouble theta)
        (Just $ P $ fmap round $ orect_offset ore)
        flips
  | otherwise = mempty

drawSprite
    :: WrappedTexture
    -> V2 WorldPos  -- ^ pos
    -> Double     -- ^ rotation in rads
    -> V2 Bool    -- ^ mirroring
    -> Renderable
drawSprite wt pos theta flips =
  drawSpriteStretched wt pos theta flips 1

playSong :: Song -> IO ()
playSong s = do
  ALUT.play [global_songs s]

mkAnim :: SF (DrawSpriteDetails Anim, V2 WorldPos) Renderable
mkAnim = proc (dsd, pos) -> do
  let anim = dsd_anim dsd
  global_tick <- round . (/ 0.1) <$> localTime -< ()
  new_anim <- onChange -< dsd_anim dsd
  anim_start <- hold 0 -< global_tick <$ new_anim

  let anim_frame = (global_tick - anim_start) `mod` frameCounts anim
  new_frame <- onChange -< anim_frame

  returnA -< \cam -> do
    for_ new_frame $ traverse_ playSound . frameSound anim
    drawSprite
      (global_anims anim !! anim_frame)
      pos
      (dsd_rotation dsd)
      (dsd_flips dsd)
      cam

mkPuppet :: SF (DrawSpriteDetails PuppetAnim, V2 WorldPos) ([AnimBox], Event (), Renderable)
mkPuppet = proc (dsd, pos) -> do
  let CannedAnim{..} = getPuppetAnim $ dsd_anim dsd
      ws = global_puppets ca_schema
  global_tick <- localTime -< ()
  new_anim <- onChange -< dsd_anim dsd
  anim_start <- hold 0 -< global_tick <$ new_anim

  let t = global_tick - anim_start

  let Just entity    = ws_schema ws ^. schemaEntity    . at _aEntity
      Just animation = entity   ^. entityAnimation . at _aAnim
      thisFrame = t * _aSpeedMult
      totalLength = animation ^. animLength
      frame = case _aRepeat || thisFrame <= totalLength of
                True -> fmod totalLength thisFrame
                False -> totalLength - 1

      is_over = thisFrame > totalLength && not _aRepeat

  returnA -< do
    case animate entity _aAnim frame of
      Nothing -> mempty
      Just rbs -> do
        let draw = foldMap (drawResultBone dsd (ws_textures ws) ca_scale pos) $ filter (not . isBone) rbs
            boxes = mapMaybe (getBox ca_scale pos) rbs
         in (boxes, bool NoEvent (Event ()) is_over, draw)


getBox :: Double -> V2 WorldPos -> ResultBone -> Maybe AnimBox
getBox sz pos rb = do
  oi <- _rbObjInfo rb
  guard $ _objInfoType oi == SpriterBox
  let dpos = sz *^ V2 (_rbX rb) (_rbY rb)
      orig_sz = V2 (_objInfoWidth oi) (_objInfoHeight oi)
      scale = V2 (_rbScaleX rb) (_rbScaleY rb)
  box <- parseBox $ T.unpack $ _objInfoName oi
  pure $ AnimBox box
       $ Rect (coerce pos + (dpos & _y %~ negate))
       $ sz *^ (orig_sz * scale)


drawResultBone
    :: DrawSpriteDetails a
    -> IntMap WrappedTexture
    -> Double  -- ^ scale
    -> V2 WorldPos
    -> ResultBone
    -> Renderable
drawResultBone dsd wts sz pos ResultBone{..}
  | dsd_flips dsd == V2 False False
  = drawSpriteStretched
      (wts IM.! (_boneObjFile $ fromJust _rbObj))
      (pos + coerce (sz *^ (V2  _rbX $ negate _rbY)))
      (dsd_rotation dsd - (_rbAngle * 180 / pi))
      (dsd_flips dsd)
      (sz *^ V2 _rbScaleX _rbScaleY)
  | dsd_flips dsd == V2 True False
  = let wt = wts IM.! (_boneObjFile $ fromJust _rbObj)
        sz' = sz *^ V2 _rbScaleX _rbScaleY
        wtsz = (fmap fromIntegral $ wt_size wt) * sz'
        ore = OriginRect wtsz (wtsz & _y .~ 0)
     in
    drawTextureOriginRect
      wt
      ore
      (pos + coerce (sz *^ (V2 (fromIntegral $ view _x $ wt_size wt) 0 - (V2  _rbX $ _rbY))))
      (dsd_rotation dsd + (_rbAngle * 180 / pi))
      (dsd_flips dsd)
      --
  | otherwise = error "NO YOU MUSTNT"


atScreenPos :: Renderable -> Renderable
atScreenPos f _ = f $ Camera 0


drawText :: Double -> V3 Word8 -> String -> V2 WorldPos -> Renderable
drawText sz color text pos@(V2 x y) cam
  | rectContains screenRect $ viaCamera cam pos
  = do
      let renderer = e_renderer $ r_engine global_resources
      for_ (zip text [0..]) $ \(c, i) -> do
        let glyph = global_glyphs c
        textureColorMod glyph $= color
        copy renderer glyph Nothing
          $ Just
          $ fmap round
          $ Rectangle (P $ coerce $ viaCamera cam $ V2 (x + coerce (i * sz)) $ withDescender sz c y)
          $ V2 sz sz
      rendererDrawBlendMode renderer $= BlendAlphaBlend
  | otherwise = mempty

withDescender :: Double -> Char -> WorldPos -> WorldPos
withDescender sz 'j' = (+ coerce sz / 6)
withDescender sz 'g' = (+ coerce sz / 5)
withDescender sz 'y' = (+ coerce sz / 6)
withDescender sz 'p' = (+ coerce sz / 6)
withDescender sz 'q' = (+ coerce sz / 6)
withDescender _  _   = id


drawParallax :: V2 WorldPos -> GameTexture -> Double -> Renderable
drawParallax sz gt scale c@(Camera cam) =
  flip atScreenPos c
    $ drawTextureOriginRect (global_textures gt) (coerce bg_ore) (logicalSize / 2) 0
    $ pure False
  where
    perc = coerce $ -cam / sz
    bg_ore = OriginRect (logicalSize ^* scale) ((logicalSize ^* scale) * perc)

#endif
