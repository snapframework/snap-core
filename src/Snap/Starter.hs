module Main where

------------------------------------------------------------------------------
import System
import System.Directory
import System.Console.GetOpt
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
data InitFlag = InitBareBones
              | InitHelp
  deriving (Show, Eq)


------------------------------------------------------------------------------
initProject :: [String] -> IO ()
initProject args = do
    case getOpt Permute options args of
      (flags, _, [])
        | InitHelp `elem` flags -> do putStrLn initUsage
                                      exitFailure
        | otherwise             -> init' (InitBareBones `elem` flags)

      (_, _, errs) -> do putStrLn $ concat errs
                         putStrLn initUsage
                         exitFailure
  where
    initUsage = unlines
        ["Usage:"
        ,""
        ,"  snap init"
        ,""
        ,"    -b  --barebones   Depend only on -core and -server"
        ,"    -h  --help        Print this message"
        ]

    options =
        [ Option ['b'] ["barebones"] (NoArg InitBareBones)
                 "Depend only on -core and -server"
        , Option ['h'] ["help"]      (NoArg InitHelp)
                 "Print this message"
        ]

    init' isBareBones = do
        cur <- getCurrentDirectory
        let dirs = splitDirectories cur
            projName = last dirs
        writeFile (projName++".cabal") (cabalFile projName isBareBones)
        createDirectory "src"
        writeFile "src/Main.hs" (mainFile isBareBones)


------------------------------------------------------------------------------
main :: IO ()
main = do
    args <- getArgs
    case args of
        ("init":args') -> initProject args'
        _              -> do putStrLn usage
                             exitFailure


------------------------------------------------------------------------------
cabalFile :: String -> Bool -> String
cabalFile projName isBareBones = unlines $
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
    ,"    snap-core >= 0.2 && <0.3,"
    ,"    snap-server >= 0.3 && <0.3,"
    ] ++ (if isBareBones then [] else ["    heist >= 0.1 && <0.2,"]) ++
    ["    filepath >= 1.1 && <1.2"
    ,""
    ,"  ghc-options: -O2 -Wall -fwarn-tabs -funbox-strict-fields -threaded -fno-warn-unused-imports"
    ]


------------------------------------------------------------------------------
mainFile :: Bool -> String
mainFile isBareBones = unlines $
    ["{-# LANGUAGE OverloadedStrings #-}"
    ,"module Main where"
    ,""
    ,"import           System"
    ,"import           Control.Applicative"
    ,"import           Control.Monad.Trans"
    ,"import           Snap.Http.Server"
    ,"import           Snap.Types"
    ,"import           Snap.Util.FileServe"
    ] ++ (if isBareBones then [] else ["import           Text.Templating.Heist"]) ++
    [""
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

