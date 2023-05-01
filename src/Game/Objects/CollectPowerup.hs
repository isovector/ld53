module Game.Objects.CollectPowerup where

import Game.Common


collectPowerup :: V2 WorldPos -> PowerupType -> Object
collectPowerup pos pt
  = id -- oscillate (\t -> coerce $ V2 0 (cos (t * 5) * 0.3))
  $ onHitByTag IsPlayer
      ( mconcat
          [ standardDeathResponse
          -- , playSoundReponse CoinSound
          , addInventoryResponse pt
          ]
      )
  $ oscillatingStaticCollisionObject (\t -> coerce $ V2 0 (cos (t * 4) * 4)) pos ore mempty
  $ drawPowerup pt ore
  where
    ore = mkCenterdOriginRect 8


drawPowerup :: PowerupType -> OriginRect Double -> V2 WorldPos -> Renderable
drawPowerup pt ore pos = mconcat
  [ drawGameTextureOriginRect (AuraTexture) (mkCenterdOriginRect $ orect_size ore * 3) pos 0 $ pure False
  , drawGameTextureOriginRect (powerupRenderable pt) ore pos 0 $ pure False
  ]


powerupRenderable :: PowerupType -> GameTexture
powerupRenderable PowerupJump = ChickenTexture
powerupRenderable PowerupDoubleJump = ChickenTexture
powerupRenderable PowerupSlide = ChickenTexture
powerupRenderable PowerupSword = ChickenTexture
