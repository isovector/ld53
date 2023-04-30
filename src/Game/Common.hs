module Game.Common
  ( module Engine.Prelude
  , module Game.Common
  , module Engine.Common
  ) where

import           Data.Maybe (isNothing)
import           Data.Monoid
import qualified Data.Set as S
import           Engine.Common
import           Engine.Prelude
import Control.Monad (guard)
import Engine.Geometry (intersects)


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

getCollisionMap :: GlobalState -> CollisionPurpose -> V2 WorldPos -> Bool
getCollisionMap gs = do
  let lev = gs_currentLevel gs
      layers = enumFromTo minBound maxBound

  \purpose -> getAny
            . foldMap ((fmap Any .) . l_hitmap lev) layers purpose
            . posToTile


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


sendDamage :: Team -> AnimBox -> Message
sendDamage t (ab_rect -> Rect pos sz) = DamageSource (Damage t 1) (coerce pos) sz


checkDamage :: Team -> [Rect Double] -> ObjectInEvents -> Event [Damage]
checkDamage t hits evs = mkEvent $ do
  (_, DamageSource d pos sz) <- fromEvent mempty $ oie_receive evs
  guard $ t /= d_team d
  guard $ any (\(Rect p s) -> intersects (Rectangle (coerce pos) sz) $ Rectangle (P p) s) hits
  pure d


mkEvent :: [a] -> Event [a]
mkEvent [] = NoEvent
mkEvent a = Event a


changeLevel :: Text -> ObjectEvents
changeLevel lvl = mempty
  & #oe_game_message .~ Event [ChangeLevel lvl]
  -- & #oe_omnipotence .~

