module Game.World where

import Engine.Types

drawWorld :: World -> Renderable
drawWorld = foldMap drawLevel . toList . w_levels

drawLevel :: Level -> Renderable
drawLevel lv = mconcat
  [ -- drawBackgroundColor $ l_bgcolor lv
    flip foldMap (enumFromTo minBound maxBound) $ l_tiles lv
  ]

tilesOf :: Rect Tile -> [V2 Tile]
tilesOf (Rect (V2 x y) (V2 w h)) = do
  dx <- [0 .. w - 1]
  dy <- [0 .. h - 1]
  pure $ V2 (x + dx) (y + dy)

