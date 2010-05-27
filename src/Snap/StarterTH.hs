{-# LANGUAGE TemplateHaskell #-}
module Snap.StarterTH where

------------------------------------------------------------------------------
import qualified Data.Foldable as F
import           Data.List
import           Language.Haskell.TH
import           System.Directory.Tree
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- Convenience types
type FileData = (String, String)
type DirData = String


------------------------------------------------------------------------------
-- Gets all the directorys in a DirTree
getDirs :: [String] -> DirTree a -> [String]
getDirs prefix (Dir n c) = (intercalate "/" (reverse (n:prefix))) : concatMap (getDirs (n:prefix)) c
getDirs _ (File _ _) = []
getDirs _ (Failed _ _) = []


------------------------------------------------------------------------------
-- Reads a directory and returns a tuple of the list of all directories
-- encountered and a list of filenames and content strings.
readTree :: String -> IO ([DirData], [FileData])
readTree dir = do
    d <- readDirectory $ dir++"/."
    let ps = zipPaths $ "" :/ (free d)
        fd = F.foldr (:) [] ps
        dirs = tail $ getDirs [] $ free d
    return $ (dirs, fd)


------------------------------------------------------------------------------
-- Calls readTree and returns it's value in a quasiquote.
dirQ :: Q Exp
dirQ = do
    d <- runIO $ readTree "project_template"
    runQ [| d |]


------------------------------------------------------------------------------
-- Creates a declaration assigning the specified name the value returned by
-- dirQ.
buildData :: String -> Q [Dec]
buildData dirName = do
    v <- valD (varP (mkName dirName))
                    (normalB dirQ)
                    []
    return [v]

