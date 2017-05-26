{-# LANGUAGE BangPatterns     #-}
{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Rank2Types       #-}
module Snap.Internal.Routing
  ( Route(..)
  , pRoute
  , route
  , routeEarliestNC
  , routeHeight
  , routeLocal
  , splitPath
  ) where
------------------------------------------------------------------------------
import           Control.Applicative      ((<|>))
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B (head, intercalate, length, null, pack, splitWith, tail)
import           Data.ByteString.Internal (c2w)
import           Data.HashMap.Strict      (HashMap)
import qualified Data.HashMap.Strict      as H (elems, empty, fromList, lookup, unionWith)
import qualified Data.Map                 as Map (empty, insertWith, unionWith)
import           Snap.Internal.Core       (MonadSnap, getRequest, getsRequest, localRequest, modifyRequest, pass, updateContextPath)
import           Snap.Internal.Http.Types (Params, Request (rqContextPath, rqParams, rqPathInfo))
import           Snap.Internal.Parsing    (urlDecode)
#if !MIN_VERSION_base(4,8,0)
import           Data.Monoid              (Monoid (..))
#endif
import           Data.Semigroup           (Semigroup (..))

------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | The internal data type you use to build a routing tree. Matching is
-- done unambiguously.
--
-- 'Capture' and 'Dir' routes can have a "fallback" route:
--
--   - For 'Capture', the fallback is routed when there is nothing to capture
--   - For 'Dir', the fallback is routed when we can't find a route in its map
--
-- Fallback routes are stacked: i.e. for a route like:
--
-- > Dir [("foo", Capture "bar" (Action bar) NoRoute)] baz
--
-- visiting the URI foo/ will result in the "bar" capture being empty and
-- triggering its fallback. It's NoRoute, so we go to the nearest parent
-- fallback and try that, which is the baz action.
data Route a m = Action ((MonadSnap m) => m a)   -- wraps a 'Snap' action
               -- captures the dir in a param
               | Capture ByteString (Route a m) (Route a m)
               -- match on a dir
               | Dir (HashMap ByteString (Route a m)) (Route a m)
               | NoRoute


------------------------------------------------------------------------------

instance Semigroup (Route a m) where
    NoRoute <> r = r

    l@(Action a) <> r = case r of
      (Action a')       -> Action (a <|> a')
      (Capture p r' fb) -> Capture p r' (fb <> l)
      (Dir _ _)         -> Dir H.empty l <> r
      NoRoute           -> l

    -- Whenever we're unioning two Captures and their capture variables
    -- differ, we have an ambiguity. We resolve this in the following order:
    --   1. Prefer whichever route is longer
    --   2. Else, prefer whichever has the earliest non-capture
    --   3. Else, prefer the right-hand side
    l@(Capture p r' fb) <> r = case r of
      (Action _)           -> Capture p r' (fb <> r)
      (Capture p' r'' fb')
              | p == p'    -> Capture p (r' <> r'') (fb <> fb')
              | rh' > rh'' -> Capture p r' (fb <> r)
              | rh' < rh'' -> Capture p' r'' (fb' <> l)
              | en' < en'' -> Capture p r' (fb <> r)
              | otherwise  -> Capture p' r'' (fb' <> l)
        where
          rh'  = routeHeight r'
          rh'' = routeHeight r''
          en'  = routeEarliestNC r' 1
          en'' = routeEarliestNC r'' 1
      (Dir rm fb')         -> Dir rm (fb' <> l)
      NoRoute              -> l

    (<>) l@(Dir rm fb) r = case r of
      (Action _)      -> Dir rm (fb <> r)
      (Capture _ _ _) -> Dir rm (fb <> r)
      (Dir rm' fb')   -> Dir (H.unionWith (<>) rm rm') (fb <> fb')
      NoRoute         -> l

instance Monoid (Route a m) where
    mempty = NoRoute
    mappend = (<>)

------------------------------------------------------------------------------
routeHeight :: Route a m -> Int
routeHeight r = case r of
  NoRoute          -> 1
  (Action _)       -> 1
  (Capture _ r' _) -> 1 + routeHeight r'
  (Dir rm _)       -> 1 + foldl max 1 (map routeHeight $ H.elems rm)
{-# INLINE routeHeight #-}


------------------------------------------------------------------------------
routeEarliestNC :: Route a m -> Int -> Int
routeEarliestNC r n = case r of
  NoRoute           -> n
  (Action _)        -> n
  (Capture _ r' _)  -> routeEarliestNC r' n+1
  (Dir _ _)         -> n
{-# INLINE routeEarliestNC #-}


------------------------------------------------------------------------------
-- | A web handler which, given a mapping from URL entry points to web
-- handlers, efficiently routes requests to the correct handler.
--
--
-- __Usage__
--
-- The URL entry points are given as relative paths, for example:
--
-- > route [ ("foo/bar/quux", fooBarQuux) ]
--
-- If the URI of the incoming request is @\/foo\/bar\/quux@ or
-- @\/foo\/bar\/quux\/...anything...@ then the request will be routed to
-- @\"fooBarQuux\"@, with 'rqContextPath' set to @\"\/foo\/bar\/quux\/\"@ and
-- 'rqPathInfo' set to @\"...anything...\"@.
--
-- A path component within an URL entry point beginning with a colon (@\":\"@)
-- is treated as a /variable capture/; the corresponding path component within
-- the request URI will be entered into the 'rqParams' parameters mapping with
-- the given name. For instance, if the routes were:
--
-- > route [ ("foo/:bar/baz", fooBazHandler) ]
--
-- Then a request for @\"\/foo\/saskatchewan\/baz\"@ would be routed to
-- @fooBazHandler@ with a mapping for @\"bar\" => \"saskatchewan\"@ in its
-- parameters table.
--
-- Longer paths are matched first, and specific routes are matched before
-- captures. That is, if given routes:
--
-- > [ ("a", h1), ("a/b", h2), ("a/:x", h3) ]
--
-- a request for @\"\/a\/b\"@ will go to @h2@, @\"\/a\/s\"@ for any /s/ will go
-- to @h3@, and @\"\/a\"@ will go to @h1@.
--
-- The following example matches @\"\/article\"@ to an article index,
-- @\"\/login\"@ to a login, and @\"\/article\/...\"@ to an article renderer.
--
-- @
-- 'route' [ (\"article\",     renderIndex)
--       , (\"article\/:id\", renderArticle)
--       , (\"login\",       'Snap.Core.method' POST doLogin) ]
-- @
--
-- __Note: URL decoding__
--
-- A short note about URL decoding: path matching and variable capture are done
-- on /decoded/ URLs, but the contents of 'rqContextPath' and 'rqPathInfo' will
-- contain the original encoded URL, i.e. what the user entered. For example,
-- in the following scenario:
--
-- > route [ ("a b c d/", foo ) ]
--
-- A request for \"@/a+b+c+d@\" will be sent to @foo@ with 'rqContextPath' set
-- to @\"/a+b+c+d/\"@.
--
-- This behaviour changed as of Snap 0.6.1; previous versions had unspecified
-- (and buggy!) semantics here.
--
--
-- __Example:__
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as Map
-- ghci> import qualified "Data.ByteString.Char8" as B8
-- ghci> import "Snap.Test"
-- ghci> :{
-- ghci| let handler = do r \<- 'getRequest'
-- ghci|                  'Snap.Core.writeBS' $ \"rqContextPath: \" \<> 'rqContextPath' r \<> \"\\n\"
-- ghci|                  'Snap.Core.writeBS' $ \"rqPathInfo: \" \<> 'rqPathInfo' r \<> \"\\n\"
-- ghci|                  'Snap.Core.writeBS' $ \"rqParams: \" \<> (B8.pack . show $ 'rqParams' r)
-- ghci| :}
-- ghci> 'Snap.Test.runHandler' ('Snap.Test.get' \"\/foo\/bar\" "Map.empty") ('route' [(\"foo\", handler)])
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Sat, 02 Aug 2014 05:16:59 GMT
--
-- rqContextPath: \/foo\/
-- rqPathInfo: bar
-- rqParams: fromList []
-- ghci> 'Snap.Test.runHandler' ('Snap.Test.get' \"\/foo\/bar\" "Map.empty") ('route' [(\"foo\/:bar\", handler)])
-- [...]
--
-- rqContextPath: \/foo\/bar\/
-- rqPathInfo:
-- rqParams: fromList [(\"bar\",[\"bar\"])]
-- @
route :: MonadSnap m => [(ByteString, m a)] -> m a
route rts = do
  p <- getsRequest rqPathInfo
  route' (return $! ()) [] (splitPath p) Map.empty rts'
  where
    rts' = mconcat (map pRoute rts)
{-# INLINE route #-}


------------------------------------------------------------------------------
-- | The 'routeLocal' function is the same as 'route', except it doesn't
-- change the request's context path. This is useful if you want to route to a
-- particular handler but you want that handler to receive the 'rqPathInfo' as
-- it is.
--
-- Example:
--
-- @
-- ghci> :set -XOverloadedStrings
-- ghci> import qualified "Data.Map" as M
-- ghci> import qualified "Data.ByteString.Char8" as B8
-- ghci> import "Snap.Test"
-- ghci> :{
-- ghci| let handler = do r \<- 'getRequest'
-- ghci|                  'Snap.Core.writeBS' $ \"rqContextPath: \" \<> 'rqContextPath' r \<> \"\\n\"
-- ghci|                  'Snap.Core.writeBS' $ \"rqPathInfo: \" \<> 'rqPathInfo' r \<> \"\\n\"
-- ghci|                  'Snap.Core.writeBS' $ \"rqParams: \" \<> (B8.pack . show $ 'rqParams' r)
-- ghci| :}
-- ghci> 'Snap.Test.runHandler' ('Snap.Test.get' \"\/foo\/bar\" M.empty) ('routeLocal' [(\"foo\", handler)])
-- HTTP\/1.1 200 OK
-- server: Snap\/test
-- date: Sat, 02 Aug 2014 05:17:28 GMT
--
-- rqContextPath: \/
-- rqPathInfo: foo\/bar
-- ghci> 'Snap.Test.runHandler' ('Snap.Test.get' \"\/foo\/bar\" M.empty) ('routeLocal' [(\"foo\/:bar\", handler)])
-- [...]
--
-- rqContextPath: \/
-- rqPathInfo: foo\/bar
-- rqParams: fromList [(\"bar\",[\"bar\"])]
-- @
routeLocal :: MonadSnap m => [(ByteString, m a)] -> m a
routeLocal rts = do
    req    <- getRequest
    let ctx = rqContextPath req
    let p   = rqPathInfo req
    let md  = modifyRequest $ \r -> r {rqContextPath=ctx, rqPathInfo=p}

    (route' md [] (splitPath p) Map.empty rts') <|> (md >> pass)

  where
    rts' = mconcat (map pRoute rts)
{-# INLINE routeLocal #-}


------------------------------------------------------------------------------
splitPath :: ByteString -> [ByteString]
splitPath = B.splitWith (== (c2w '/'))
{-# INLINE splitPath #-}


------------------------------------------------------------------------------
pRoute :: MonadSnap m => (ByteString, m a) -> Route a m
pRoute (r, a) = foldr f (Action a) hier
  where
    hier   = filter (not . B.null) $ B.splitWith (== (c2w '/')) r
    f s rt = if B.head s == c2w ':'
        then Capture (B.tail s) rt NoRoute
        else Dir (H.fromList [(s, rt)]) NoRoute
{-# INLINE pRoute #-}


------------------------------------------------------------------------------
route' :: MonadSnap m
       => m ()           -- ^ action to run before we call the user handler
       -> [ByteString]   -- ^ the \"context\"; the list of path segments we've
                         -- already successfully matched, in reverse order
       -> [ByteString]   -- ^ the list of path segments we haven't yet matched
       -> Params
       -> Route a m
       -> m a
route' pre !ctx _ !params (Action action) =
    localRequest (updateContextPath (B.length ctx') . updateParams)
                 (pre >> action)
  where
    ctx' = B.intercalate (B.pack [c2w '/']) (reverse ctx)
    updateParams req = req
      { rqParams = Map.unionWith (flip (++)) params (rqParams req) }

route' pre !ctx [] !params (Capture _ _  fb) =
    route' pre ctx [] params fb

route' pre !ctx paths@(cwd:rest) !params (Capture p rt fb)
    | B.null cwd = fallback
    | otherwise  = m <|> fallback
  where
    fallback = route' pre ctx paths params fb
    m = maybe pass
              (\cwd' -> let params' = Map.insertWith (flip (++)) p [cwd'] params
                        in route' pre (cwd:ctx) rest params' rt)
              (urlDecode cwd)

route' pre !ctx [] !params (Dir _ fb) =
    route' pre ctx [] params fb
route' pre !ctx paths@(cwd:rest) !params (Dir rtm fb) = do
    cwd' <- maybe pass return $ urlDecode cwd
    case H.lookup cwd' rtm of
      Just rt -> (route' pre (cwd:ctx) rest params rt) <|>
                 (route' pre ctx paths params fb)
      Nothing -> route' pre ctx paths params fb

route' _ _ _ _ NoRoute = pass
