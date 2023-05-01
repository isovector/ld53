module Game.Common
  ( module Engine.Prelude
  , module Game.Common
  , module Engine.Common
  , hash
  ) where

import           Control.Monad (guard)
import           Data.Hashable (hash)
import           Data.List (partition)
import           Data.Maybe (isNothing)
import qualified Data.Set as S
import           Engine.Common
import           Engine.Geometry (intersects, rectContains)
import           Engine.Prelude
import           Game.Objects.Particle (particle)


withLifetime :: Double -> Object -> Object
withLifetime dur sf = proc oi -> do
  ev <- nowish () -< ()
  t <- time -< ()
  start <- hold 0 -< t <$ ev
  die <- edge -< start + dur < t
  oo <- sf -< oi
  returnA -< oo & #oo_events . #oe_die <>~ die


onHitBy :: ObjectTag -> ObjectInput -> Event ObjectId
onHitBy otag oi = do
  hits <- oie_hit $ oi_events oi
  case find (any $ S.member otag . os_tags) hits of
    Just x0 -> pure $ fst x0
    Nothing -> noEvent

playerHitRectObj'
    :: (SF ObjectInput ObjectEvents)
    -> OriginRect Double
    -> Color
    -> V2 WorldPos
    -> Object
playerHitRectObj' msg ore col pos =
  playerHitRectObj (msg >>> arr (, ())) ore (const $ drawOriginRect col ore) pos

playerHitRectObj
    :: (SF ObjectInput (ObjectEvents, a))
    -> OriginRect Double
    -> (a -> V2 WorldPos -> Renderable)
    -> V2 WorldPos
    -> Object
playerHitRectObj msg ore r pos =
  proc oi -> do
    (evs, a) <- msg -< oi

    returnA -< ObjectOutput
      { oo_events = evs
      , oo_render = r a $ os_pos $ oi_state oi
      , oo_state = (noObjectState pos)
          { os_collision = Just $ coerce ore
          }
      }

charging :: Time -> SF ObjectInput Bool -> SF ObjectInput (Double, Event Double)
charging dur while = proc oi -> do
  maxed <- after dur 1 -< ()
  t <- sscan (+) 0 -< fi_dt $ oi_frameInfo oi
  x <- while -< oi
  released <- edge -< x
  let prog = t / dur

  let done = mergeEvents
              [ maxed
              , prog <$ released
              ]

  returnA -< (prog , done)

data RateLimited a = RateLimited
  { rl_cooldown_left :: Maybe Time
  , rl_on_refresh :: Event ()
  , rl_data :: a
  }
  deriving (Eq, Ord, Show, Generic)


rl_available :: RateLimited a -> Bool
rl_available = isNothing . rl_cooldown_left


rateLimit :: Time -> SF (Event a, b) c -> SF (Event a, b) (RateLimited c)
rateLimit cooldown sf = loopPre 0 $ proc ((ev, b), last_ok) -> do
  t <- time -< ()
  let next_alive = t + cooldown
      ok = t >= last_ok

  let actually_die = whenE ok ev
  respawn_at <- hold 0 -< next_alive <$ actually_die

  let alive = respawn_at < t

  out <- sf -< (actually_die, b)

  respawn <- edge -< respawn_at <= t
  returnA -< (RateLimited (bool (Just $ respawn_at - t) Nothing alive) respawn out, respawn_at)


select :: SF a Bool -> SF a b -> SF a b -> SF a b
select sf t f = proc a -> do
  isTrue <- sf -< a
  tb <- t -< a
  fb <- f -< a
  returnA -< bool fb tb isTrue

holdFor :: Time -> SF (Event a) (Maybe a)
holdFor dur = proc ev -> do
  t <- time -< ()
  startTime <- hold (-100) -< t <$ ev
  e <- hold (error "holdFor") -< ev
  let isHeld = t < startTime + dur
  returnA -< bool Nothing (Just e) isHeld


onlyOncePer :: Time -> SF (Event a) (Event a)
onlyOncePer dur = proc ev -> do
  fmap rl_data $ rateLimit dur (arr fst) -< (ev, ())


sendDamage :: Team -> V2 WorldPos -> [AnimBox] -> ObjectEvents
sendDamage team src boxes =
  let (_, hurts) = splitAnimBoxes boxes
      send t (ab_rect -> Rect pos sz) = SendDamageSource $ DamageSource src (Damage t 1) (coerce pos) sz
   in mempty & #oe_game_message .~ Event (fmap (send team) hurts)


checkDamage :: Team -> V2 WorldPos -> [Rect Double] -> ObjectInput -> Event (V2 Double, [Damage])
checkDamage t me hits oi = coerce $ fmap sequence $ mkEvent $ do
  DamageSource src d pos sz <- gs_damage_set $ gameState oi
  guard $ t /= d_team d
  guard $ any (\(Rect p s) -> intersects (Rectangle (coerce pos) sz) $ Rectangle (P p) s) hits
  pure (normalize $ me - src, d)


checkDamage' :: Team -> V2 WorldPos -> [AnimBox] -> ObjectInput -> Event (V2 Double, [Damage])
checkDamage' t me boxes =
  let (hits, _) = splitAnimBoxes boxes
   in checkDamage t me $ fmap ab_rect hits


mkEvent :: [a] -> Event [a]
mkEvent [] = NoEvent
mkEvent a = Event a


splitAnimBoxes :: [AnimBox] -> ([AnimBox], [AnimBox])
splitAnimBoxes = partition ((== Hitbox) . ab_type)


damageHandler
      :: Team
      -> SF (ObjectInput, OriginRect Double, [AnimBox])
            (ObjectEvents, Maybe (V2 Double), Event (), Int -> Int)
damageHandler team = proc (oi, ore, boxes) -> do
  let os = oi_state oi
      OriginRect sz _ = ore
      pos = os_pos os

  let dmg_in_ev = fmap (second $ sum . fmap d_damage) $ checkDamage' team pos boxes oi
  both <- onlyOncePer 0.1 -< dmg_in_ev
  let dmg_ev = fmap snd both
      dir = eventToMaybe $ fmap (normalize . fst) both

  let dmg = event 0 id dmg_ev
  let hp' = os_hp os - dmg
  die <- edge -< hp' <= 0

  returnA -<
    ( sendDamage team pos boxes
          & #oe_spawn .~ fmap
              (pure . dmgIndicator (pos - V2 0 20 - (coerce sz & _x .~ 0)))
              dmg_ev
    , dir
    , die
    , event id subtract dmg_ev
    )


dmgIndicator :: V2 WorldPos -> Int -> Object
dmgIndicator pos dmg =
  particle pos (V2 0 (-30)) (arr $ drawText 6 (V3 255 0 0) $ '-' : show dmg) 0 2


mkHurtHitBox :: V2 WorldPos -> OriginRect Double -> [AnimBox]
mkHurtHitBox pos ore =
  let rect = originRectToRect2 ore $ coerce pos
   in [ AnimBox Hitbox rect
      , AnimBox Hurtbox rect
      ]

mkHitBox :: V2 WorldPos -> OriginRect Double -> [AnimBox]
mkHitBox pos ore =
  let rect = originRectToRect2 ore $ coerce pos
   in [ AnimBox Hitbox rect
      ]

mkHurtBox :: V2 WorldPos -> OriginRect Double -> [AnimBox]
mkHurtBox pos ore =
  let rect = originRectToRect2 ore $ coerce pos
   in [ AnimBox Hurtbox rect
      ]

pauseWhenOffscreen :: Object -> Object
pauseWhenOffscreen obj = proc oi -> do
  pause (ObjectOutput mempty mempty $ noObjectState 0)
    ( proc oi -> do
        t <- time -< ()
        on_start <- lessNowish () -< ()
        start_time <- hold 100 -< t <$ on_start
        let pos = os_pos $ oi_state oi
        let should_run = t < start_time || rectContains (fi_active_level $ frameInfo oi) pos
        returnA -< not should_run
    ) obj -< oi

spawnTime :: SF a Time
spawnTime = proc _ -> do
  on_start <- nowish () -< ()
  t <- time -< ()
  start <- hold 0 -< t <$ on_start
  returnA -< t - start


addInventoryResponse :: PowerupType -> SF (Event a) ObjectEvents
addInventoryResponse
  = response #oe_game_message
  . const
  . pure
  . AddInventory

