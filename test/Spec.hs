{-# LANGUAGE OverloadedStrings #-}

-- |

module Main where

import qualified Data.Text.IO as T
import           Hsmv
import           Test.Hspec

main = hspec spec

spec =
  describe
    "Sanity check"
    (it
       "Replacements in file"
       (do out <- T.readFile "test/SanityOut.hs"
           shouldReturn
             (do contents <- T.readFile "test/Sanity.hs"
                 pure (applyToContents "Foo" "Bar" contents))
             out))
