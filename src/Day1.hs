module Day1 where

import Text.ParserCombinators.Parsec

type Freq = Int
type Drift = Int

pdrifts :: Parser [Drift]
pdrifts = endBy pdrift eol

eol :: Parser Char
eol = char '\n'

pdrift :: Parser Drift
pdrift = do
  sign <- oneOf "+-"
  digits <- many1 digit
  let value = read digits
    in return (if sign == '+' then value else (-value))

parseDrifts :: String -> Either ParseError [Drift]
parseDrifts = parse pdrifts "(unknown)"

applyDrift :: Freq -> Drift -> Freq
applyDrift start drift = start + drift

calcDrift :: Freq -> [Drift] -> Freq
calcDrift = foldr applyDrift
