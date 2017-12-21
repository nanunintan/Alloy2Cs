module Main where

import System.IO
import Alloy.Parser
import CD.Pretty
import Transform.Alloy2CD

main :: IO ()
main = do alloy <- hGetContents stdin
          putStrLn (pretty $ alloy2cd $ parseString alloy)

