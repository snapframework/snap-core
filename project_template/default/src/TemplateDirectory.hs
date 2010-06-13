module TemplateDirectory
    ( TemplateDirectory
    , newTemplateDirectory
    , newTemplateDirectory'

    , getTemplateState
    , reloadTemplateDirectory

    , bindSplices
    , withSplices
    ) where
import           Control.Concurrent
import           Control.Monad
import           Control.Monad.Trans
import           Data.ByteString.Char8 (ByteString)
import           Text.Templating.Heist
import           Text.Templating.Heist.Splices.Static


data TemplateDirectory m
    = TemplateDirectory
        FilePath
        (TemplateState m)
        (MVar (TemplateState m))
        StaticTagState


newTemplateDirectory :: (MonadIO m, MonadIO n)
                     => FilePath
                     -> TemplateState m
                     -> n (Either String (TemplateDirectory m))
newTemplateDirectory dir templateState = liftIO $ do
    (origTs,sts) <- bindStaticTag templateState
    ets <- loadTemplates dir origTs
    leftPass ets $ \ts -> do
        tsMVar <- newMVar $ ts
        return $ TemplateDirectory dir origTs tsMVar sts


newTemplateDirectory' :: (MonadIO m, MonadIO n)
                      => FilePath
                      -> TemplateState m
                      -> n (TemplateDirectory m)
newTemplateDirectory' = ((either fail return =<<) .) . newTemplateDirectory


getTemplateState :: (Monad m, MonadIO n)
                 => TemplateDirectory m
                 -> n (TemplateState m)
getTemplateState (TemplateDirectory _ _ tsMVar _) = liftIO $ readMVar $ tsMVar


reloadTemplateDirectory :: (MonadIO m, MonadIO n)
                        => TemplateDirectory m
                        -> n (Either String ())
reloadTemplateDirectory (TemplateDirectory p origTs tsMVar sts) = liftIO $ do
    clearStaticTagCache sts
    ets <- loadTemplates p origTs
    leftPass ets $ \ts -> modifyMVar_ tsMVar (const $ return ts)


bindSplices :: Monad m
           => [(ByteString, Splice m)]
           -> TemplateState m
           -> TemplateState m
bindSplices = flip $ foldr (uncurry bindSplice)


withSplices :: MonadIO m
            => [(ByteString, Splice m)]
            -> TemplateState m
withSplices = foldr (uncurry bindSplice) emptyTemplateState


leftPass :: Monad m => Either String b -> (b -> m c) -> m (Either String c)
leftPass e m = either (return . Left . loadError) (liftM Right . m) e
  where
    loadError = (++) "Error loading templates: "
