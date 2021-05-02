module TinyThreePassCompiler where

import           Data.Functor                   ( ($>) )
import           Data.List                      ( foldl' )
import           Data.Maybe                     ( fromJust )
import           Text.ParserCombinators.Parsec
                                         hiding ( digit )

data AST = Imm Int
         | Arg Int
         | Add AST AST
         | Sub AST AST
         | Mul AST AST
         | Div AST AST
         deriving (Eq, Show)

data Token = TChar Char
           | TInt Int
           | TStr String
           deriving (Eq, Show)

alpha, digit :: String
alpha = ['a' .. 'z'] ++ ['A' .. 'Z']
digit = ['0' .. '9']

tokenize :: String -> [Token]
tokenize [] = []
tokenize xxs@(c : cs) | c `elem` "-+*/()[]" = TChar c : tokenize cs
                      | not (null i)        = TInt (read i) : tokenize is
                      | not (null s)        = TStr s : tokenize ss
                      | otherwise           = tokenize cs
 where
  (i, is) = span (`elem` digit) xxs
  (s, ss) = span (`elem` alpha) xxs

compile :: String -> [String]
compile = pass3 . pass2 . pass1


pass1 :: String -> AST
pass1 x = case regularParse parseFunc x of
  Right (LFun args expr) -> toAST expr (extractArgs args)
  Right expr             -> toAST expr []
  Left  err              -> error (show err)

pass2 :: AST -> AST
pass2 = reduce

pass3 :: AST -> [String]
pass3 = assemble

-- parser

data Lang = Atom String
          | LNum Int
          | LFun [Lang] Lang
          | LAdd Lang Lang
          | LSub Lang Lang
          | LMul Lang Lang
          | LDiv Lang Lang
          deriving (Show, Eq)

regularParse :: Parser a -> String -> Either ParseError a
regularParse p = parse p "(unknown)"

parseFunc :: Parser Lang
parseFunc = do
  char '[' >> spaces
  args <- option [] $ alphas `sepEndBy` many1 space
  char ']' >> spaces
  LFun (Atom <$> args) <$> parseExpr

parseInt :: Parser Lang
parseInt = LNum . read <$> digits <|> Atom <$> alphas

parseExpr :: Parser Lang
parseExpr = parseTerm `chainl1` addop

parseTerm :: Parser Lang
parseTerm = parseFactor `chainl1` mulop

parseFactor :: Parser Lang
parseFactor = betSpaces $ between (char '(') (char ')') parseExpr <|> parseInt

mulop :: Parser (Lang -> Lang -> Lang)
mulop = betSpaces (char '*') $> LMul <|> betSpaces (char '/') $> LDiv

addop :: Parser (Lang -> Lang -> Lang)
addop = betSpaces (char '+') $> LAdd <|> betSpaces (char '-') $> LSub

betSpaces :: Parser a -> Parser a
betSpaces = between spaces spaces

digits :: Parser String
digits = many1 (satisfy (`elem` digit))

alphas :: Parser String
alphas = many1 (satisfy (`elem` alpha))

-- morph to AST

toAST :: Lang -> [(Lang, AST)] -> AST
toAST (LFun args expr) _     = toAST expr (extractArgs args)
toAST lang             table = toAST' lang
 where
  toAST' x@(Atom _  ) = replace x table
  toAST' (  LNum i  ) = Imm i
  toAST' (  LAdd a b) = Add (toAST' a) (toAST' b)
  toAST' (  LSub a b) = Sub (toAST' a) (toAST' b)
  toAST' (  LMul a b) = Mul (toAST' a) (toAST' b)
  toAST' (  LDiv a b) = Div (toAST' a) (toAST' b)
  toAST' _            = undefined

extractArgs :: [Lang] -> [(Lang, AST)]
extractArgs args = zip args (zipWith (\x _ -> Arg x) [0 ..] args)

replace :: Lang -> [(Lang, AST)] -> AST
replace x table = fromJust (lookup x table)

-- pass2

reduce :: AST -> AST
reduce a@(Arg _  ) = a
reduce i@(Imm _  ) = i
reduce (  Add x y) = case Add (reduce x) (reduce y) of
  Add (Imm a) (Imm b) -> Imm (a + b)
  z                   -> z
reduce (Sub x y) = case Sub (reduce x) (reduce y) of
  Sub (Imm a) (Imm b) -> Imm (a - b)
  z                   -> z
reduce (Mul x y) = case Mul (reduce x) (reduce y) of
  Mul (Imm a) (Imm b) -> Imm (a * b)
  z                   -> z
reduce (Div x y) = case Div (reduce x) (reduce y) of
  Div (Imm a) (Imm b) -> Imm (a `div` b)
  z                   -> z

-- pass3

assemble :: AST -> [String]
assemble (Imm i  ) = ["IM " ++ show i]
assemble (Arg a  ) = ["AR " ++ show a]
assemble (Add x y) = assemble x <> ["PU"] <> assemble y <> ["SW", "PO", "AD"]
assemble (Sub x y) = assemble x <> ["PU"] <> assemble y <> ["SW", "PO", "SU"]
assemble (Mul x y) = assemble x <> ["PU"] <> assemble y <> ["SW", "PO", "MU"]
assemble (Div x y) = assemble x <> ["PU"] <> assemble y <> ["SW", "PO", "DI"]

-- others

simulate :: [String] -> [Int] -> Int
simulate asm argv = takeR0 $ foldl' step (0, 0, []) asm where
  step (r0, r1, stack) ins = case ins of
    ('I' : 'M' : xs) -> (read xs, r1, stack)
    ('A' : 'R' : xs) -> (argv !! n, r1, stack) where n = read xs
    "SW"             -> (r1, r0, stack)
    "PU"             -> (r0, r1, r0 : stack)
    "PO"             -> (head stack, r1, tail stack)
    "AD"             -> (r0 + r1, r1, stack)
    "SU"             -> (r0 - r1, r1, stack)
    "MU"             -> (r0 * r1, r1, stack)
    ~"DI"            -> (r0 `div` r1, r1, stack)
  takeR0 (r0, _, _) = r0
