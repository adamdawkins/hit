{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Main where

import Control.Monad
import Data.Maybe
import Data.Functor ((<&>))
import Data.Text as T hiding (map, filter)
import Data.Tree
import Shelly as S hiding (FilePath)
import System.Environment
import System.Directory


default (T.Text)

runCommand :: String -> [String] -> Sh ()
runCommand "init"    args = initCmd $ listToMaybe args
runCommand "commit"  _    = commitCmd
runCommand cmd _          =  echo $ pack $ cmd ++ " is not a hit command"

-- initCmd:
-- hit init [directory]
-- [x] create a .git directory
-- [x] create objects and refs directories
-- [x] add a HEAD file to refs
-- [x] if a directory is not provided use the current directory
initCmd :: Maybe String -> Sh ()
initCmd dir = 
            mkdir_p workingDir
            >> cd workingDir
            >> mkdirTree (".git" # leaves ["objects", "refs"])
            >> touchfile ".git/refs/HEAD"
        where (#) = Node
              leaves = map (# [])
              workingDir = case dir of
                Just dir -> toFilePath dir
                Nothing ->  toFilePath "."
              toFilePath = fromText . pack

commitCmd :: Sh ()
commitCmd = do
  currentPath <- pwd
  files <- S.findWhen test_f currentPath
  -- let files = filter test_f dir 
  relFiles <- traverse relPath files
  let relText = fmap (toTextIgnore) relFiles
  let contents = filter (not . (T.isPrefixOf ".git")) relText
  let contents' = filter (not . (T.isPrefixOf ".stack-work")) contents
  
  mapM_ echo contents'

main = do
  (command:args) <- getArgs
  shelly $ runCommand command args
