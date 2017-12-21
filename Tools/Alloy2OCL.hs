module Main where

import Alloy.Parser
import Transform.Alloy2OCL
import OCL.Pretty
import System.IO

main :: IO ()
main = do alloy <- hGetContents stdin
          case alloy2ocl (parseString alloy) of Left error -> putStrLn $ "Error: " ++ error
                                                Right ocl  -> putStrLn (OCL.Pretty.pretty ocl)

