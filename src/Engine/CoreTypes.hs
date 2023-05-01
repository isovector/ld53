module Engine.CoreTypes
  ( module Engine.CoreTypes
  , module SDL
  ) where

import SDL (V2(..), Rectangle(..), Point(..))
import Data.Hashable (Hashable)
import Data.Text (Text)
import Control.DeepSeq (NFData)
import SDL.Vect


newtype Tile = Tile
  { getTile :: Int
  }
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, NFData, Hashable)


newtype Pixel = Pixel
  { getPixel :: Int
  }
  deriving newtype (Eq, Ord, Show, Read, Enum, Bounded, Num, NFData)


newtype ScreenPos = ScreenPos
  { getScreenPos :: Double
  }
  deriving newtype (Eq, Ord, Show, Read, Enum, Num, Fractional, Floating, Real, RealFrac, NFData)


newtype WorldPos = WorldPos
  { getWorldPos :: Double
  }
  deriving newtype (Eq, Ord, Show, Read, Enum, Num, Fractional, Floating, Real, RealFrac, Hashable, NFData, Epsilon)

instance Semigroup WorldPos where
  (<>) = (+)

instance Monoid WorldPos where
  mempty = 0



data ObjectId
  = StaticId Text
  | DynamicId Int
  deriving stock (Show, Read)
  deriving stock (Eq, Ord)

