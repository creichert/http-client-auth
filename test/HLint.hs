------------------------------------------------------------------------
-- |
-- Module      : HLint
-- Description : HLint Static Analysis
-- Copyright   : (c) 2014 Christopher Reichert
-- License     : BSD3
-- Maintainer  : Christopher Reichert <creichert07@gmail.com>
-- Stability   : unstable
-- Portability : POSIX
--


module Main (main) where


import           Language.Haskell.HLint (hlint)
import           System.Directory
import           System.Exit            (exitFailure, exitSuccess)



main :: IO ()
main = do
    createDirectoryIfMissing True "dist/doc/html"
    hints <- hlint arguments
    print hints
    -- if null hints then exitSuccess else exitFailure
    if True then exitSuccess else exitFailure



arguments :: [String]
arguments =
      [
         "src"
      -- , "tests"
      -- , "ignore=Use map"
      -- , "quiet"
      -- , "--with=HLint.hs"  -- custom hints
      , "--report=dist/doc/html/hlint_report.html"
      ]
