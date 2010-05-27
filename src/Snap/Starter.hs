{-# LANGUAGE TemplateHaskell #-}
module Main where

------------------------------------------------------------------------------
import Data.List
import qualified Data.Text as T
import System
import System.Directory
import System.FilePath
------------------------------------------------------------------------------

import Snap.StarterTH


------------------------------------------------------------------------------
-- Creates a value tDir :: ([String], [(String, String)])
$(buildData "tDir")


------------------------------------------------------------------------------
usage :: String
usage = unlines
    ["Usage:"
    ,""
    ,"  snap <action>"
    ,""
    ,"    <action> can be one of:"
    ,"      init - create a new project directory structure in the current directory"
    ]


------------------------------------------------------------------------------
data InitFlag = InitBareBones
              | InitHelp
  deriving (Show, Eq)


setup :: String -> IO ()
setup projName = do
    mapM createDirectory (fst tDir)
    mapM_ write (snd tDir)
  where
    write (f,c) =
        if isSuffixOf "foo.cabal" f
          then writeFile (projName++".cabal") (insertProjName $ T.pack c)
          else writeFile f c
    insertProjName c = T.unpack $ T.replace
                           (T.pack "projname")
                           (T.pack projName) c

------------------------------------------------------------------------------
initProject :: IO ()
initProject = do
    cur <- getCurrentDirectory
    let dirs = splitDirectories cur
        projName = last dirs
    setup projName


------------------------------------------------------------------------------
main :: IO ()
main = do
    args <- getArgs
    case args of
        ("init":_) -> initProject
        _          -> do putStrLn usage
                         exitFailure

