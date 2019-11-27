module Day1 where

import           Text.ParserCombinators.Parsec
import qualified Data.Set                      as Set

type Freq = Int
type Drift = Int

pdrifts :: Parser [Drift]
pdrifts = endBy pdrift eol

eol :: Parser Char
eol = char '\n'

pdrift :: Parser Drift
pdrift = do
  sign   <- oneOf "+-"
  digits <- many1 digit
  let value = read digits in return (if sign == '+' then value else (-value))

parseDrifts :: String -> Either ParseError [Drift]
parseDrifts = parse pdrifts "(unknown)"

calcDrift ::[Drift] -> Freq
calcDrift = last . freqs

freqs :: [Drift] -> [Freq]
freqs = scanl (+) 0

findFirstDup :: [Drift] -> Freq
findFirstDup = record Set.empty . freqs . cycle
 where
  record seen drifts | x `Set.member` seen = x
                     | otherwise           = record (x `Set.insert` seen) xs
    where (x, xs) = (head drifts, tail drifts)
