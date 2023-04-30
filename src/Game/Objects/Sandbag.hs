module Game.Objects.Sandbag where

import Game.Common


sandbag :: V2 WorldPos -> OriginRect Double -> Object
sandbag pos ore = proc oi -> do
  let OriginRect sz off = ore

  on_start <- nowish () -< ()
  let def =
        (noObjectState pos)
          { os_collision = Just ore
          }
  let os = event (oi_state oi) (const def) on_start

  let dmg_ev = checkDamage OtherTeam [Rect (coerce pos - off) sz] $ oi_events oi
--   hits <- oi_events oi
--   case find (any $ S.member otag . os_tags) hits of
--     Just x0 -> pure $ fst x0
--     Nothing -> noEvent

  returnA -<
    ObjectOutput
      { oo_events = mempty
      , oo_render = drawOriginRect (event (V4 255 255 255 92) (const $ V4 255 0 0 92) dmg_ev) ore pos
      , oo_state = os
      }


