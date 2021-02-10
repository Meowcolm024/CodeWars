module Brainfuck where

import Data.Either (fromRight)
import Text.ParserCombinators.Parsec

type Op = Char

data Bf = Atom [Op] | Loop [Bf]

data Pointer = Pointer {pos :: Int, mem :: [Int]} deriving (Show)

type Act = Machine -> Machine

data Machine = Machine
  { ptr :: Pointer,
    input :: String,
    output :: String
  }

brainfuck :: String -> String -> String
brainfuck code inp = output $ walkBf (fromRight [] $ eval code) (Machine (Pointer 0 (replicate 2048 0)) inp "")

modPtr :: (Pointer -> Pointer) -> Machine -> Machine
modPtr f (Machine p i o) = Machine (f p) i o

modPos :: (Int -> Int) -> Pointer -> Pointer
modPos f (Pointer p m) = Pointer (f p) m

modMem :: (Int -> Int) -> Pointer -> Pointer
modMem f (Pointer p m) =
  let (lp, rp) = splitAt p m
   in Pointer p (lp ++ [f $ head rp] ++ tail rp)

eval :: String -> Either ParseError [Bf]
eval = regularParse parseExpr . filter (`elem` "+-<>[].,")

-- Parser

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p "(unknown)"

parseLoop :: Parser Bf
parseLoop = Loop <$> (char '[' *> parseExpr <* char ']')

parseAtom :: Parser Bf
parseAtom = Atom <$> (many1 . oneOf) "+-<>.,"

parseExpr :: Parser [Bf]
parseExpr = many1 $ choice [try parseLoop, parseAtom]

-- eval

act :: Op -> Act
act = \a -> case a of
  '+' -> modPtr (modMem (ascend True))
  '-' -> modPtr (modMem (ascend False))
  '<' -> modPtr (modPos (\x -> x - 1))
  '>' -> modPtr (modPos (\x -> x + 1))
  ',' -> \(Machine p (i : is) o) -> Machine (modMem (const (fromEnum i)) p) is o
  '.' -> \(Machine p is o) -> Machine p is (o ++ [toEnum (mem p !! pos p)])
  _ -> error "Invalid inst"
  where
    ascend True i = if i == 255 then 0 else i + 1
    ascend False i = if i == 0 then 255 else i -1

walk :: Machine -> Bf -> Machine
walk pt (Atom ops) = foldl (flip ($)) pt (map act ops)
walk pt lp@(Loop lps) =
  if (mem (ptr pt) !! pos (ptr pt)) == 0
    then pt
    else flip walk lp $ walkBf lps pt

walkBf :: [Bf] -> Machine -> Machine
walkBf = flip $ foldl walk
