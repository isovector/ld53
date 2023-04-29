module Game.Box where

import Game.Types
import Text.Read (readMaybe)

parseBox :: String -> Maybe BoxType
parseBox = readMaybe . takeWhile (/= '_')

