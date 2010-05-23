module Snap.Internal.Routing where


------------------------------------------------------------------------------
import           Control.Applicative ((<|>))
import           Data.ByteString (ByteString)
import           Data.ByteString.Internal (c2w)
import qualified Data.ByteString as B
import           Data.Monoid
import qualified Data.Map as Map

------------------------------------------------------------------------------
import           Snap.Internal.Http.Types
import           Snap.Internal.Types


------------------------------------------------------------------------------
{-|

The internal data type you use to build a routing tree.  Matching is
done unambiguously.

'Capture' and 'Dir' routes can have a "fallback" route:

  - For 'Capture', the fallback is routed when there is nothing to capture
  - For 'Dir', the fallback is routed when we can't find a route in its map

Fallback routes are stacked: i.e. for a route like:

> Dir [("foo", Capture "bar" (Action bar) NoRoute)] baz

visiting the URI foo/ will result in the "bar" capture being empty and
triggering its fallback. It's NoRoute, so we go to the nearest parent
fallback and try that, which is the baz action.

-}
data Route a = Action (Snap a)                        -- wraps a 'Snap' action
             | Capture ByteString (Route a) (Route a) -- captures the dir in a param
             | Dir (Map.Map ByteString (Route a)) (Route a)  -- match on a dir
             | NoRoute


------------------------------------------------------------------------------
instance Monoid (Route a) where
    mempty = NoRoute

    -- Unions two routes, favoring the right-hand side
    mappend NoRoute r = r

    mappend l@(Action _) r = case r of
      (Action _)        -> r
      (Capture p r' fb) -> Capture p r' (mappend fb l)
      (Dir _ _)         -> mappend (Dir Map.empty l) r
      NoRoute           -> l

    mappend l@(Capture p r' fb) r = case r of
      (Action _)           -> Capture p r' (mappend fb r)
      (Capture p' r'' fb')
               | p == p'   -> Capture p (mappend r' r'') (mappend fb fb')
               | otherwise -> r
      (Dir rm fb')         -> Dir rm (mappend fb' l)
      NoRoute              -> l

    mappend l@(Dir rm fb) r = case r of
      (Action _)      -> Dir rm (mappend fb r)
      (Capture _ _ _) -> Dir rm (mappend fb r)
      (Dir rm' fb')   -> Dir (Map.unionWith mappend rm rm') (mappend fb fb')
      NoRoute         -> l


------------------------------------------------------------------------------
-- | A web handler which, given a mapping from URL entry points to web
-- handlers, efficiently routes requests to the correct handler.
--
-- The URL entry points are given as relative paths, for example:
--
-- > route [ ("foo/bar/quux", fooBarQuux) ]
--
-- If the URI of the incoming request is
--
-- > /foo/bar/quux
--
-- or
--
-- > /foo/bar/quux/...anything...
--
-- then the request will be routed to \"@fooBarQuux@\", with 'rqContextPath'
-- set to \"@\/foo\/bar\/quux\/@\" and 'rqPathInfo' set to
-- \"@...anything...@\".
--
-- A path component within an URL entry point beginning with a colon (\"@:@\")
-- is treated as a /variable capture/; the corresponding path component within
-- the request URI will be entered into the 'rqParams' parameters mapping with
-- the given name. For instance, if the routes were:
--
-- > route [ ("foo/:bar/baz", fooBazHandler) ]
--
-- Then a request for \"@\/foo\/saskatchewan\/baz@\" would be routed to
-- @fooBazHandler@ with a mapping for:
--
-- > "bar" => "saskatchewan"
--
-- in its parameters table.
--
-- Longer paths are matched first, and specific routes are matched before
-- captures. That is, if given routes:
--
-- > [ ("a", h1), ("a/b", h2), ("a/:x", h3) ]
--
-- a request for \"@\/a\/b@\" will go to @h2@, \"@\/a\/s@\" for any /s/ will go
-- to @h3@, and \"@\/a@\" will go to @h1@.
--
-- The following example matches \"@\/article@\" to an article index,
-- \"@\/login@\" to a login, and \"@\/article\/...@\" to an article renderer.
--
-- > route [ ("article",     renderIndex)
-- >       , ("article/:id", renderArticle)
-- >       , ("login",       method POST doLogin) ]
--
route :: [(ByteString, Snap a)] -> Snap a
route rts = route' (return ()) rts' []
  where
    rts' = mconcat (map pRoute rts)


------------------------------------------------------------------------------
-- | The 'routeLocal' function is the same as 'route', except it doesn't change
-- the request's context path. This is useful if you want to route to a
-- particular handler but you want that handler to receive the 'rqPathInfo' as
-- it is.
routeLocal :: [(ByteString, Snap a)] -> Snap a
routeLocal rts' = do
    req    <- getRequest
    let ctx = rqContextPath req
    let p   = rqPathInfo req
    let md  = modifyRequest $ \r -> r {rqContextPath=ctx, rqPathInfo=p}

    route' md rts []   <|>   (md >> pass)

  where
    rts = mconcat (map pRoute rts')

          
------------------------------------------------------------------------------
pRoute :: (ByteString, Snap a) -> Route a
pRoute (r, a) = foldr f (Action a) hier
  where
    hier   = filter (not . B.null) $ B.splitWith (== (c2w '/')) r
    f s rt = if B.head s == c2w ':'
        then Capture (B.tail s) rt NoRoute
        else Dir (Map.fromList [(s, rt)]) NoRoute


------------------------------------------------------------------------------
route' :: Snap ()               -- ^ an action to be run before any user
                                -- handler
       -> Route a               -- ^ currently active routing table
       -> [Route a]             -- ^ list of fallback routing tables in case
                                -- the current table fails
       -> Snap a
route' pre (Action action) _ = pre >> action

route' pre (Capture param rt fb) fbs = do
    cwd <- getRequest >>= return . B.takeWhile (/= (c2w '/')) . rqPathInfo
    if B.null cwd
      then route' pre fb fbs
      else do localRequest (updateContextPath (B.length cwd) . (f cwd)) $
                           route' pre rt (fb:fbs)
  where
    f v req = req { rqParams = Map.insertWith (++) param [v] (rqParams req) }

route' pre (Dir rtm fb) fbs = do
    cwd <- getRequest >>= return . B.takeWhile (/= (c2w '/')) . rqPathInfo
    case Map.lookup cwd rtm of
      Just rt -> do
          localRequest (updateContextPath (B.length cwd)) $
                       route' pre rt (fb:fbs)
      Nothing -> route' pre fb fbs

route' _ NoRoute       []   = pass
route' pre NoRoute (fb:fbs) = route' pre fb fbs
