module Alloy.Test where

import Test.HUnit
import Text.ParserCombinators.Parsec
import System.Directory
import Alloy.Parser

testParse :: String -> Assertion
testParse file =  do p <- parseFromFile parser file
                     case p of Left err -> assertFailure $ "Parse error at file " ++ file ++ ": " ++ (show err)
                               Right val -> return ()

dir = "Alloy/Examples/"

isAls :: String -> Bool
isAls s = dropWhile (/='.') s == ".als"

main :: IO ()
main = do files <- getDirectoryContents dir
          let test = TestList $ map (TestCase . testParse) $ map (dir++) $ filter isAls files
          runTestTT test
          return ()

