module Day03 where

import Data.Maybe (fromJust)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char.Lexer (decimal)

data Op
  = Mul Int Int
  | Do
  | Dont
  deriving (Eq)

type Parser = Parsec Void String

solve :: String -> (Int, Int)
solve input = (partOne, partTwo)
  where
    ops = pOps input
    partOne = sum $ map mul ops
    partTwo = sum $ map mul $ filterOps ops
    mul op = case op of
      Mul a b -> a * b
      _ -> 0

pOps :: String -> [Op]
pOps input = fromJust $ parseMaybe (many ((try . skipUntil) (pMul <|> pDo <|> pDont)) <* some anySingle) input

filterOps :: [Op] -> [Op]
filterOps [] = []
filterOps (op : ops) = case op of
  Do -> let (muls, rest) = span (/= Dont) ops in filter (/= Do) muls ++ filterOps rest
  Dont -> filterOps $ dropWhile (/= Do) ops
  _ -> op : filterOps ops

skipUntil :: Parser a -> Parser a
skipUntil p = try p <|> (anySingle *> skipUntil p)

pMul :: Parser Op
pMul = Mul <$ string "mul(" <*> decimal <* char ',' <*> decimal <* char ')'

pDo :: Parser Op
pDo = Do <$ string "do()"

pDont :: Parser Op
pDont = Dont <$ string "don't()"
