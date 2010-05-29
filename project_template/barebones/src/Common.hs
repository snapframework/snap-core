{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

module Common where

import           Control.Concurrent
import           Control.Exception (SomeException)
import           Control.Monad
import           Control.Monad.CatchIO
import           Prelude hiding (catch)
import           Snap.Http.Server
import           Snap.Types
import           Snap.Util.GZip
import           System
import           System.Posix.Env

setLocaleToUTF8 :: IO ()
setLocaleToUTF8 = do
    mapM_ (\k -> setEnv k "en_US.UTF-8" True)
          [ "LANG"
          , "LC_CTYPE"
          , "LC_NUMERIC"
          , "LC_TIME"
          , "LC_COLLATE"
          , "LC_MONETARY"
          , "LC_MESSAGES"
          , "LC_PAPER"
          , "LC_NAME"
          , "LC_ADDRESS"
          , "LC_TELEPHONE"
          , "LC_MEASUREMENT"
          , "LC_IDENTIFICATION"
          , "LC_ALL" ]

data AppConfig = AppConfig {
    accessLog :: Maybe FilePath,
    errorLog :: Maybe FilePath
}

quickServer :: AppConfig -> Snap () -> IO ()
quickServer config siteHandlers = do
    args   <- getArgs
    port   <- case args of
                []       -> error "You must specify a port!" >> exitFailure
                (port:_) -> return $ read port

    setLocaleToUTF8

    (try $ httpServe "*" port "myserver"
             (accessLog config)
             (errorLog config)
             (withCompression siteHandlers))
             :: IO (Either SomeException ())

    threadDelay 1000000
    putStrLn "exiting"
    return ()

