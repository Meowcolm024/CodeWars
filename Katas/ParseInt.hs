module ParseInt where

import Control.Monad (void)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec

parseInt :: String -> Int
parseInt n = case regularParse parseNum (dropAnd n) of
  Right v -> v
  Left _ -> 0

dropAnd :: String -> String
dropAnd = unwords . filter (/= "and") . words

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p "(unknown)"

parseTens :: Parser Int
parseTens = do
  first <- choice $ map (try . string) lessHund
  void $ optionMaybe $ char '-'
  next <- option "zero" $ choice $ map (try . string) $ drop 8 lessHund
  return $ lookupNum first + lookupNum next

parseHundred :: Parser Int
parseHundred = do
  left <- option 0 $ parseTens
  void $ string " hundred"
  void $ optionMaybe $ char ' '
  right <- option 0 $ parseTens
  return $ left * 100 + right

parseThousand :: Parser Int
parseThousand = do
  left <- option 0 $ choice [try parseHundred, parseTens]
  void $ optionMaybe $ char ' '
  void $ string "thousand"
  void $ optionMaybe $ char ' '
  right <- option 0 $ choice [try parseHundred, parseTens]
  return $ left * 1000 + right

parseMillion :: Parser Int
parseMillion = do
  left <- option 0 $ choice [try parseThousand, try parseHundred, parseTens]
  void $ optionMaybe $ char ' '
  void $ string "million"
  void $ optionMaybe $ char ' '
  right <- option 0 $ choice [try parseThousand, try parseHundred, parseTens]
  return $ left * 1000000 + right

parseNum :: Parser Int
parseNum = choice [try parseMillion, try parseThousand, try parseHundred, parseTens]

lookupNum :: String -> Int
lookupNum v = case Map.lookup v dict of
  Just num -> num
  Nothing -> undefined

dict :: Map.Map String Int
dict =
  Map.fromList
    [ ("zero", 0),
      ("one", 1),
      ("two", 2),
      ("three", 3),
      ("four", 4),
      ("five", 5),
      ("six", 6),
      ("seven", 7),
      ("eight", 8),
      ("nine", 9),
      ("ten", 10),
      ("eleven", 11),
      ("twelve", 12),
      ("thirteen", 13),
      ("fourteen", 14),
      ("fifteen", 15),
      ("sixteen", 16),
      ("seventeen", 17),
      ("eighteen", 18),
      ("nineteen", 19),
      ("twenty", 20),
      ("thirty", 30),
      ("forty", 40),
      ("fifty", 50),
      ("sixty", 60),
      ("seventy", 70),
      ("eighty", 80),
      ("ninety", 90),
      ("hundred", 100),
      ("thousand", 1000),
      ("million", 1000000)
    ]

lessHund :: [String]
lessHund =
  [ "twenty",
    "thirty",
    "forty",
    "fifty",
    "sixty",
    "seventy",
    "eighty",
    "ninety",
    "zero",
    "one",
    "two",
    "three",
    "fourteen",
    "four",
    "five",
    "sixteen",
    "seventeen",
    "eighteen",
    "nineteen",
    "six",
    "seven",
    "eight",
    "nine",
    "ten",
    "eleven",
    "twelve",
    "thirteen",
    "fifteen"
  ]