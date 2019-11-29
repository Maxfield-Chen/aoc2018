module Day2 where

import           Text.ParserCombinators.Parsec
import qualified Data.Map                      as M

pIds :: Parser [String]
pIds = endBy pId eol

pId :: Parser String
pId = many1 letter

eol :: Parser Char
eol = char '\n'

type Checksum = (Int, Int)

freqs :: String -> M.Map Char Int
freqs = foldl (\m c -> M.insertWith (+) c 1 m) M.empty

letterAppearsN :: Int -> M.Map Char Int -> Int
letterAppearsN n id = if ret then 1 else 0
  where ret = foldl (\ret n' -> ret || (n' == n)) False id

genChecksum :: [String] -> Checksum
genChecksum = foldl
  (\(threeCount, fourCount) id ->
    ( threeCount + letterAppearsN 2 (freqs id)
    , fourCount + letterAppearsN 3 (freqs id)
    )
  )
  (0, 0)

printCheckSum :: IO ()
printCheckSum = do
  idData <- readFile "/home/nihliphobe/projects/haskell/aoc2018/data/day2.txt"
  case parse pIds "(unknown)" idData of
    Left err -> fail (show err)
    Right ids -> print $ threeCount * fourCount 
      where (threeCount, fourCount) = genChecksum ids
