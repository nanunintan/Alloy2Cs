module Main where

import Alloy.Pretty
import Transform.CD2Alloy
import CD.Parser
import System.IO

main :: IO ()
main = do cd <- hGetContents stdin
          case cd2alloy (parseString cd) of Left error  -> putStrLn $ "Error: " ++ error
                                            Right alloy -> putStrLn (pretty alloy)
