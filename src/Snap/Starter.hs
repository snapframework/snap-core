module Main where

------------------------------------------------------------------------------
import System
import System.Directory
import System.FilePath.Posix
------------------------------------------------------------------------------


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
initProject :: IO ()
initProject = do
    cur <- getCurrentDirectory
    let dirs = splitDirectories cur
        projName = last dirs
    writeFile (projName++".cabal") (cabalFile projName)
    createDirectory "src"
    writeFile "src/Main.hs" mainFile


------------------------------------------------------------------------------
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["init"] -> initProject
        _        -> do putStrLn usage
                       exitFailure


------------------------------------------------------------------------------
cabalFile :: String -> String
cabalFile projName = unlines
    ["Name:                "++projName
    ,"Version:             0.1"
    ,"Synopsis:            Project Synopsis Here"
    ,"Description:         Project Description Here"
    ,"License:             AllRightsReserved"
    ,"Author:              Author"
    ,"Maintainer:          maintainer@example.com"
    ,"Stability:           Experimental"
    ,"Category:            Web"
    ,"Build-type:          Simple"
    ,"Cabal-version:       >=1.2"
    ,""
    ,"Executable "++projName
    ,"  hs-source-dirs: src"
    ,"  main-is: Main.hs"
    ,""
    ,"  Build-depends:"
    ,"    base >= 4,"
    ,"    haskell98,"
    ,"    monads-fd >= 0.1 && <0.2,"
    ,"    bytestring >= 0.9.1 && <0.10,"
    ,"    snap-core >= 0.1 && <0.2,"
    ,"    snap-server >= 0.1 && <0.2,"
    ,"    heist >= 0.1 && <0.2,"
    ,"    filepath >= 1.1 && <1.2"
    ,""
    ,"  ghc-options: -O2 -Wall -fwarn-tabs -funbox-strict-fields -threaded -fno-warn-unused-imports"
    ,""
    ,"  Extensions: OverloadedStrings"
    ]


------------------------------------------------------------------------------
mainFile :: String
mainFile = unlines
    ["module Main where"
    ,""
    ,"import           System"
    ,"import           Control.Applicative"
    ,"import           Control.Monad.Trans"
    ,"import           Snap.Http.Server"
    ,"import           Snap.Types"
    ,"import           Snap.Util.FileServe"
    ,"import           Text.Templating.Heist"
    ,""
    ,"site :: Snap ()"
    ,"site ="
    ,"    ifTop (writeBS \"hello world\") <|>"
    ,"    fileServe \".\""
    ,""
    ,"main :: IO ()"
    ,"main = do"
    ,"    args <- getArgs"
    ,"    let port = case args of"
    ,"                   []  -> 8000"
    ,"                   p:_ -> read p"
    ,"    httpServe \"*\" port \"myserver\""
    ,"        (Just \"access.log\")"
    ,"        (Just \"error.log\")"
    ,"        site"
    ]

