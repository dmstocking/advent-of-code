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
replaceNumbers ('0':rest) = 0 : replaceNumbers rest
replaceNumbers ('1':rest) = 1 : replaceNumbers rest
replaceNumbers ('2':rest) = 2 : replaceNumbers rest
replaceNumbers ('3':rest) = 3 : replaceNumbers rest
replaceNumbers ('4':rest) = 4 : replaceNumbers rest
replaceNumbers ('5':rest) = 5 : replaceNumbers rest
replaceNumbers ('6':rest) = 6 : replaceNumbers rest
replaceNumbers ('7':rest) = 7 : replaceNumbers rest
replaceNumbers ('8':rest) = 8 : replaceNumbers rest
replaceNumbers ('9':rest) = 9 : replaceNumbers rest
replaceNumbers a@('z':'e':'r':'o':rest) = 0 : replaceNumbers (drop 1 a)
replaceNumbers a@('o':'n':'e':rest) = 1 : replaceNumbers (drop 1 a)
replaceNumbers a@('t':'w':'o':rest) = 2 : replaceNumbers (drop 1 a)
replaceNumbers a@('t':'h':'r':'e':'e':rest) = 3 : replaceNumbers (drop 1 a)
replaceNumbers a@('f':'o':'u':'r':rest) = 4 : replaceNumbers (drop 1 a)
replaceNumbers a@('f':'i':'v':'e':rest) = 5 : replaceNumbers (drop 1 a)
replaceNumbers a@('s':'i':'x':rest) = 6 : replaceNumbers (drop 1 a)
replaceNumbers a@('s':'e':'v':'e':'n':rest) = 7 : replaceNumbers (drop 1 a)
replaceNumbers a@('e':'i':'g':'h':'t':rest) = 8 : replaceNumbers (drop 1 a)
replaceNumbers a@('n':'i':'n':'e':rest) = 9 : replaceNumbers (drop 1 a)
replaceNumbers (_:rest) = replaceNumbers rest
replaceNumbers [] = []

firstAndLast :: [Int] -> Int
firstAndLast list = head list * 10 + last list
