module Main where

import OCL.Parser as OCL
import CD.Parser as CD
import Alloy.Pretty
import Transform.OCL2Alloy
import System.Environment
import System.IO

main :: IO ()
main = do args <- getArgs
          if (length args /= 1) then putStrLn "UML class diagram required!"
          else do cd <- readFile (head args)
                  ocl <- hGetContents stdin
                  case ocl2alloy (CD.parseString cd) (OCL.parseString ocl) of Left error  -> putStrLn $ "Error: " ++ error
                                                                              Right alloy -> putStrLn (pretty alloy)
