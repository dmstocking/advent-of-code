{-# LANGUAGE OverloadedStrings #-}

import Data.Text (toLower, pack, unpack)

main :: IO ()
main = do
  contents <- readFile "Day1.test"
  putStrLn "Day1.test answer"
  putStrLn (sumCalibration contents)
  contents <- readFile "Day1.input"
  putStrLn "Day1.input answer"
  putStrLn (sumCalibration contents)

sumCalibration :: String -> String
sumCalibration = show . sum . map (firstAndLast . replaceNumbers) . lines . unpack . toLower . pack

replaceNumbers :: String -> [Int]
replaceNumbers [] = []
replaceNumbers str = digit str ++ replaceNumbers (drop 1 str)

digit :: String -> [Int]
digit ('0':rest) = [0]
digit ('1':rest) = [1]
digit ('2':rest) = [2]
digit ('3':rest) = [3]
digit ('4':rest) = [4]
digit ('5':rest) = [5]
digit ('6':rest) = [6]
digit ('7':rest) = [7]
digit ('8':rest) = [8]
digit ('9':rest) = [9]
digit ('z':'e':'r':'o':rest) = [0]
digit ('o':'n':'e':rest) = [1]
digit ('t':'w':'o':rest) = [2]
digit ('t':'h':'r':'e':'e':rest) = [3]
digit ('f':'o':'u':'r':rest) = [4]
digit ('f':'i':'v':'e':rest) = [5]
digit ('s':'i':'x':rest) = [6]
digit ('s':'e':'v':'e':'n':rest) = [7]
digit ('e':'i':'g':'h':'t':rest) = [8]
digit ('n':'i':'n':'e':rest) = [9]
digit _ = []

firstAndLast :: [Int] -> Int
firstAndLast list = head list * 10 + last list
