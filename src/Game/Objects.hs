{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

module Game.Objects where

import           Control.Applicative (optional)
import           Control.Error (note)
import           Control.Lens (Prism')
import           Control.Monad.Error (Error)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (mapMaybe, fromMaybe)
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable (for)
import           Engine.Importer (ldtkColorToColor)
import           Engine.Types
import           Engine.Utils (tileToPos)
import           Game.Objects.Antagonist (antagonist)
import           Game.Objects.BreakableWall (breakableWall)
import           Game.Objects.Checkpoint (checkpoint)
import           Game.Objects.DespawnTrigger (despawnTrigger)
import           Game.Objects.Elevator (elevator)
import           Game.Objects.ParticleSpawner (particleSpawner)
import           Game.Objects.Player (player)
import           Game.Objects.Sandbag (sandbag)
import           Game.Objects.Slime (slime)
import           Game.Objects.SpawnTrigger (spawnTrigger)
import           Game.Objects.TextBillboard (textBillboard)
import           Game.Objects.Thrower (thrower)
import           Game.Objects.TutorialRegion (tutorialRegion)
import           Game.Objects.Unknown (unknown)
import qualified LDtk.Types as LDtk


buildEntity
    :: Text
    -> V2 WorldPos
    -> OriginRect Double
    -> Map Text LDtk.FieldValue
    -> Map Text Object
    -> Either Text Object
buildEntity "Player" pos _ props _ =
  player pos
    <$> fmap (fromMaybe []) (optional $ asEnumArray "Player" "powerups" props)
buildEntity "Antagonist" pos _ _ _ = pure $ antagonist pos
buildEntity "Thrower" pos ore props _ =
  thrower pos ore
    <$> asEnum "Thrower" "projectile" props
    <*> optional (asPos "Thrower" "path" props)
    <*> asDouble "Thrower" "cooldown" props
buildEntity "Checkpoint" pos _ _ _ = pure $ checkpoint pos
buildEntity "TutorialRegion" pos ore props _ =
  tutorialRegion pos ore
    <$> asText "TutorialRegion" "key" props
    <*> fmap fromIntegral (asInt "TutorialRegion" "timer" props)
buildEntity "BreakableWall" pos ore _ _ =
  pure $ breakableWall pos ore
buildEntity "Elevator" pos ore _ _ =
  pure $ elevator pos ore
buildEntity "Sandbag" pos ore _ _ =
  sandbag pos
    <$> pure ore
buildEntity "Slime" pos ore props _ =
  slime pos ore
    <$> optional (asPos "Slime" "path" props)
buildEntity "Text" pos _ props _ =
  textBillboard
    <$> optional (asDouble "Text" "duration" props)
    <*> asDouble "Text" "size" props
    <*> asColor "Text" "color" props
    <*> asText "Text" "text" props
    <*> pure pos
buildEntity "ParticleSpawner" pos _ props _ = do
  pt <- asEnum "ParticleSpawner" "type" props
  pure $ particleSpawner pos pt
buildEntity "SpawnTrigger" pos sz props refmap = do
  persistent <- asBool "SpawnTrigger" "persistent" props
  refs <- getRefs "SpawnTrigger" "refs" props refmap
  pure $ spawnTrigger pos sz persistent refs
buildEntity "DespawnTrigger" pos sz props refmap = do
  refs <- getRefs "DespawnTrigger" "despawn" props refmap
  pure $ despawnTrigger pos sz $ S.map StaticId $ M.keysSet refs
buildEntity nm pos sz _ _ = do
  traceM $ "unregistered entity: " <> T.unpack nm
  pure $ unknown nm pos sz

instance Error Text where


as :: Text -> (Prism' LDtk.FieldValue a) -> Text -> Text -> Map Text LDtk.FieldValue -> Either Text a
as ty pris obj field m
  | Just (preview pris -> Just v) <- M.lookup field m = Right v
  | Just v <- M.lookup field m = Left $ mconcat
      [ obj
      , "/"
      , field
      , " had the wrong type ("
      , T.pack $ show v
      , ") but wanted "
      , ty
      ]
  | otherwise = Left $ mconcat
      [ obj
      , "/"
      , field
      , " was not found"
      ]

getRefs :: Text -> Text -> Map Text LDtk.FieldValue -> Map Text Object -> Either Text (Map Text Object)
getRefs obj prop props refs = do
  z <- as "Array" #_ArrayValue obj prop props
  iids <- for z $ note "couldn't lookup ref" . fmap (view #entityIid) . preview #_EntityRefValue
  pure $ foldMap (\iid -> M.singleton iid $ refs M.! iid) iids

asPos :: Text -> Text -> Map Text LDtk.FieldValue -> Either Text (V2 WorldPos)
asPos = fmap (fmap (fmap gridToWorld)) . as "Text" #_PointValue

gridToWorld :: LDtk.GridPoint -> V2 WorldPos
-- TODO(sandy): EXTREME HACK
-- the editor gives us this coordinate in CURRENT GRID SIZE
-- which is 8 lol (half of the tile size)
gridToWorld (LDtk.GridPoint cx cy) = (/ 2) $ tileToPos $ coerce $ V2 cx cy

asText :: Text -> Text -> Map Text LDtk.FieldValue -> Either Text Text
asText = as "Text" #_StringValue

asDouble :: Text -> Text -> Map Text LDtk.FieldValue -> Either Text Double
asDouble = fmap (fmap (fmap realToFrac)) . asFloat

asFloat :: Text -> Text -> Map Text LDtk.FieldValue -> Either Text Float
asFloat = as "Float" #_FloatValue

asColor :: Text -> Text -> Map Text LDtk.FieldValue -> Either Text Color
asColor = fmap (fmap (fmap ldtkColorToColor)) . as "Color" #_ColorValue

asEnum :: Read a => Text -> Text -> Map Text LDtk.FieldValue -> Either Text a
asEnum = fmap (fmap (fmap $ read . T.unpack)) . as "Enum" #_EnumValue

asEnumArray :: Read a => Text -> Text -> Map Text LDtk.FieldValue -> Either Text [a]
asEnumArray = fmap (fmap (fmap (mapMaybe (fmap (read . T.unpack) . preview #_EnumValue )))) . as "Array" #_ArrayValue

asBool :: Text -> Text -> Map Text LDtk.FieldValue -> Either Text Bool
asBool = as "Int" #_BooleanValue

asInt :: Text -> Text -> Map Text LDtk.FieldValue -> Either Text Integer
asInt = as "Int" #_IntegerValue

