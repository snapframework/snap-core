{-# LANGUAGE OverloadedStrings #-}
module Glue
    ( templateHandler
    , defaultReloadHandler
    , templateServe
    , render
    ) where

import           Control.Applicative
import           Control.Monad
import           Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as B
import           Prelude hiding (catch)
import           Snap.Types hiding (dir)
import           Snap.Util.FileServe
import           Text.Templating.Heist
import           Text.Templating.Heist.TemplateDirectory


templateHandler :: MonadSnap m
                => TemplateDirectory m
                -> (TemplateDirectory m -> m ())
                -> (TemplateState m -> m ())
                -> m ()
templateHandler td reload f = reload td <|> (f =<< getDirectoryTS td)


defaultReloadHandler :: MonadSnap m => TemplateDirectory m -> m ()
defaultReloadHandler td = path "admin/reload" $ do
    e <- reloadTemplateDirectory td
    modifyResponse $ setContentType "text/plain; charset=utf-8"
    writeBS . B.pack $ either id (const "Templates loaded successfully.") e


render :: MonadSnap m => TemplateState m -> ByteString -> m ()
render ts template = do
    bytes <- renderTemplate ts template
    flip (maybe pass) bytes $ \x -> do
        modifyResponse $ setContentType "text/html; charset=utf-8"
        writeBS x


templateServe :: MonadSnap m => TemplateState m -> m ()
templateServe ts = ifTop (render ts "index") <|> do
    path' <- getSafePath
    when (head path' == '_') pass
    render ts $ B.pack path'
