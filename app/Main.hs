{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Rename modules.

module Main where

import           Control.Monad
import           Data.Semigroup ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Hsmv
import           Options.Applicative.Simple
import           System.Directory
import           System.FilePath
import           System.IO

--------------------------------------------------------------------------------
-- Main entry point

data Config =
  Config
    { configFrom :: Text
    , configTo :: Text
    , configFromPath :: FilePath
    , configToPath :: FilePath
    , configModules :: [FilePath]
    , configDryRun :: !Bool
    } deriving (Show)

main :: IO ()
main = do
  (config, ()) <-
    simpleOptions
      "0"
      "Haskell module rename"
      "This program renames modules."
      (Config <$>
       fmap T.pack (strOption (long "from" <> help "The original module name")) <*>
       fmap T.pack (strOption (long "to" <> help "The target module name")) <*>
       strOption (long "from-path" <> help "The original module path") <*>
       strOption (long "to-path" <> help "The target module path") <*>
       some (strArgument (metavar "MODULEPATH" <> help "Filepath for a module")) <*>
       flag
         False
         True
         (long "dry-run" <>
          help "Don't make changes, just print what would be done."))
      empty
  mapM_ (rename config) (configModules config)
  unless
    (configDryRun config)
    (do exists <- doesFileExist (configFromPath config)
        if exists
          then do
            createDirectoryIfMissing
              True
              (takeDirectory (configFromPath config))
            renameFile (configFromPath config) (configToPath config)
          else hPutStrLn
                 stderr
                 ("Warning: File does not exist: " ++ configFromPath config))

--------------------------------------------------------------------------------
-- Commands

rename :: Config -> FilePath -> IO ()
rename config path = do
  contents <- T.readFile path
  if configDryRun config
    then T.putStrLn
           (applyToContents (configFrom config) (configTo config) contents)
    else do T.writeFile
              path
              (applyToContents (configFrom config) (configTo config) contents)
