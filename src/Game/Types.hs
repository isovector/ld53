{-# LANGUAGE DeriveAnyClass #-}
module Game.Types where

import Data.Map (Map)
import Data.Set (Set)
import Engine.CoreTypes
import GHC.Generics (Generic)
import Generics.Deriving.Enum


data GameState = GameState
  { gs_inventory :: Set PowerupType
  , gs_dyn_col :: Map ObjectId (Set CollisionPurpose, Rectangle WorldPos)
  , gs_player_loc :: V2 WorldPos
  , gs_damage_set :: [DamageSource]
  , gs_game_over :: Maybe GameOverReason
  }
  deriving stock Generic

data CollisionPurpose
  = CollisionWall
  | CollisionGround
  | CollisionCeiling
  | CollisionCheckGround
  | CollisionOnElevator
  deriving (Eq, Ord, Show, Enum, Bounded, Read, Generic)

data GameOverReason
  = GORReset
  | GORDeath
  | GORWin
  deriving (Eq, Ord, Show, Enum, Bounded, Read, Generic)

data GameMessage
  = AddInventory PowerupType
  | RegisterDynCollision ObjectId (Set CollisionPurpose) (Rectangle WorldPos)
  | UnregisterDynCollision ObjectId
  | SetPlayerLocation (V2 WorldPos)
  | SendDamageSource DamageSource
  | GameOver GameOverReason
  deriving stock (Eq, Ord, Show, Read, Generic)

data DamageSource =
  DamageSource
    (V2 WorldPos)  -- origin
    Damage
    (V2 WorldPos) (V2 Double)  -- rect
  deriving stock (Eq, Ord, Show, Read, Generic)

------------------------------------------------------------------------------
-- | Textures used by the game.
data GameTexture
    = NintendoLogo
    | LightningTexture
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
    | JumpMedalTexture
    | DoubleJumpMedalTexture
    | SlideMedalTexture
    | SwordMedalTexture
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
  = Layer1 | Layer2 | City | Layer3
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
  = PowerupJump
  | PowerupDoubleJump
  | PowerupSword
  | PowerupSlide
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
  | SlimeIdle
  | DraculaIdle
  | DraculaActive
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (GEnum)

data Message
  = SetCheckpoint (V2 WorldPos)
  | CurrentCheckpoint ObjectId
  | Die
  | RecoverHealth
  deriving stock (Eq, Ord, Show, Read, Generic)

data Damage = Damage
  { d_team :: Team
  , d_damage :: Int
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

data Team = PlayerTeam | OtherTeam
  deriving stock (Eq, Ord, Show, Read, Generic)

data ParticleType
  = Gore
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
  | PlayerRun
  | PlayerTakeoff
  | PlayerJump
  | PlayerJumpStab
  | PlayerFall
  | PlayerFallSlice
  | PlayerDucking
  | PlayerDucked
  | PlayerUnducking
  | PlayerDuckStab
  | PlayerSlidePrep
  | PlayerSlide
  | PlayerAirSlide
  | PlayerKnockback
  | PlayerDie
  deriving stock (Eq, Ord, Show, Read, Generic, Enum, Bounded)

data BoxType
  = Hitbox
  | Hurtbox
  deriving stock (Eq, Ord, Show, Read, Generic)

