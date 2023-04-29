{-# LANGUAGE RecordWildCards #-}

module Engine.Bones where

import Data.Spriter.Skeleton
import Data.Spriter.Types
import Control.Lens
import Engine.Types
import Engine.FRP


makeSprites :: Schema -> [FilePath]
makeSprites schema = fmap toProp $ schema ^. schemaFolder . _head . folderFile
  where
    toProp :: File -> FilePath
    toProp File{..} = "art/raw/" <> _fileName

-- drawArt :: ()
--         -> Time  -- ^ Time since start of game.
--         -> Renderable
-- drawArt Art{ _aCanned = CannedAnim{..}
--            , _aStarted
--            } correction now =
--   let Just entity    = _aSchema ^. schemaEntity    . at _aEntity
--       Just animation = entity   ^. entityAnimation . at _aAnim
--       thisFrame = (now - _aStarted) * _aSpeedMult
--       totalLength = animation ^. animLength
--       frame = case _aRepeat || thisFrame <= totalLength of
--                 True -> fmod totalLength thisFrame
--                 False -> totalLength - 1
--       sprites = makeSprites _aSchema correction
--       drawBone ResultBone{..} = move (V2 _rbX $ -_rbY)
--                               . rotate (-_rbAngle)
--                               . group
--                               . return
--                               . scaleXY _rbScaleX _rbScaleY
--    in case animate animation frame of
--         Just (filter (not . isBone) -> objs) ->
--           group $ fmap (\x -> drawBone x $ sprites ! (_boneObjFile . fromJust $ _rbObj x)) objs
--         Nothing -> blank
