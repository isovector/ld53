{-# LANGUAGE DeriveAnyClass #-}
module Game.Types where

import Data.Set (Set)
import Engine.CoreTypes
import GHC.Generics (Generic)
import Generics.Deriving.Enum


data GameState = GameState
  { gs_coins :: Int
  , gs_inventory :: Set PowerupType
  , gs_end :: Bool
  , gs_deaths :: Int
  }
  deriving stock Generic

data GameMessage
  = AddCoin
  | AddInventory PowerupType
  | GameWon
  | AddPlayerDeath
  deriving stock (Eq, Ord, Show, Read, Generic)

------------------------------------------------------------------------------
-- | Textures used by the game.
data GameTexture
    = NintendoLogo
    | ChickenTexture
    | Parallax0
    | Parallax1
    | Parallax2
    | ChargeTexture
    | TeleTexture
    | AuraTexture
    | TrampolineTexture
    | KeycapTexture
    | CheckpointTexture
    | ActiveCheckpointTexture
    | EggTexture
    | ArrowTexture
  deriving (Eq, Ord, Show, Read, Enum, Bounded)


data WorldName = GameWorld | HelpWorld
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

------------------------------------------------------------------------------
-- | Audio used by the game.
data Sound
  deriving (Eq, Ord, Show, Read)

data Song
  deriving (Eq, Ord, Show, Read)

data LevelLayer
  = Layer1 | Layer2 | Layer3
  deriving (Eq, Ord, Show, Read, Enum, Bounded)

------------------------------------------------------------------------------
-- | Input for the frame.
data Controls = Controls
  { c_space :: Bool
  , c_jump :: Bool
  , c_slide :: Bool
  , c_attack :: Bool
  , c_reset :: Bool
  , c_full_restart :: Bool
  , c_dir :: V2 Int
  }
  deriving (Eq)

instance Semigroup Controls where
  Controls a1 b1 c1 d1 e1 f1 xy1 <> Controls a2 b2 c2 d2 e2 f2 xy2
    = Controls
        { c_space = a1 || a2
        , c_jump = b1 || b2
        , c_slide = c1 || c2
        , c_attack = d1 || d2
        , c_reset = e1 || e2
        , c_full_restart = f1 || f2
        , c_dir = if xy2 /= 0 then xy2 else xy1
        }

instance Monoid Controls where
  mempty = defaultControls

defaultControls :: Controls
defaultControls = Controls
  { c_space = False
  , c_reset = False
  , c_full_restart = False
  , c_jump = False
  , c_slide = False
  , c_attack = False
  , c_dir = V2 0 0
  }

tileSize :: Num a => a
tileSize = 16

data PowerupType
  = PowerupDoubleJump
  | PowerupWarpBall
  | PowerupTotsugeki
  deriving (Eq, Ord, Show, Enum, Bounded, Generic, Read)

data ObjectTag
  = IsPlayer
  | IsTileEntity
  deriving (Eq, Ord, Show, Generic)

logicalSize :: RealFrac a => V2 a
logicalSize = V2 (h * aspectRatio) h
  where
    h = 270

aspectRatio :: RealFrac a => a
aspectRatio = 16 / 9

screenRect :: (RealFrac a) => Rectangle a
screenRect = Rectangle (P $ -tileSize * buffer) (logicalSize + tileSize * 2 * buffer)
  where
    buffer = 4

data Sprite
  = MainCharacter
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving anyclass (GEnum)


data Anim
  = Idle Sprite
  | NoAnim Sprite
  | Run Sprite
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (GEnum)

data Message
  = TeleportTo (V2 WorldPos)
  | TeleportOpportunity (V2 WorldPos)
  | SetCheckpoint (V2 WorldPos)
  | OnTrampoline Double
  | PlayerDeath
  | Die
  | CurrentCheckpoint ObjectId
  deriving stock (Eq, Ord, Show, Read, Generic)

data ParticleType
  = Gore
  | Firework
  deriving stock (Eq, Ord, Show, Read, Generic, Enum, Bounded)

data TileData
  = Twinkle Int
  deriving stock (Eq, Ord, Show, Read, Generic)

data PuppetName
  = BallerPuppet
  | ManPuppet
  deriving stock (Eq, Ord, Show, Read, Generic, Enum, Bounded)

data PuppetAnim
  = BallerDribble
  | BallerRun
  | PlayerIdleNoSword
  | PlayerIdleSword
  | PlayerGrabSword
  | PlayerStab
  deriving stock (Eq, Ord, Show, Read, Generic, Enum, Bounded)

data BoxType
  = Hitbox
  | Hurtbox
  deriving stock (Eq, Ord, Show, Read, Generic)

