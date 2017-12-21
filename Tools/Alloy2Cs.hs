module Main where

import System.IO
import Alloy.Parser
import Cs.Pretty
import Transform.Alloy2Cs

main :: IO ()
main = do alloy <- hGetContents stdin
          putStrLn (pretty $ alloy2cs $ parseString alloy)
