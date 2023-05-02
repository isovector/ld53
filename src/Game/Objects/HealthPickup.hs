module Game.Objects.HealthPickup where

import qualified Data.Set as S
import Game.Common


healthPickup :: V2 WorldPos -> Object
healthPickup pos
  = id -- oscillate (\t -> coerce $ V2 0 (cos (t * 5) * 0.3))
  $ onHit (Just . fmap fst . filter (S.member IsPlayer . os_tags . snd)) (respondWith RecoverHealth)
  $ onHitByTag IsPlayer
      ( mconcat
          [ standardDeathResponse
          -- , playSoundReponse CoinSound
          ]
      )
  $ oscillatingStaticCollisionObject (\t -> coerce $ V2 0 (cos (t * 4) * 4)) pos ore mempty
  $ drawPowerup ore
  where
    ore = mkCenterdOriginRect 8


drawPowerup :: OriginRect Double -> V2 WorldPos -> Renderable
drawPowerup ore pos = mconcat
  [ --drawGameTextureOriginRect (AuraTexture) (mkCenterdOriginRect $ orect_size ore * 3) pos 0 $ pure False
  drawGameTextureOriginRect ChickenTexture (mkCenterdOriginRect $ orect_size ore * 3) pos 0 $ pure False
  ]



