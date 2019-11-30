module Day2 where

import           Text.ParserCombinators.Parsec
import Data.Maybe as Maybe
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

lettersDiffering :: String -> String -> Int
lettersDiffering = countDiff 0
 where
  countDiff n a b | null a || null b = n
                  | x == x'          = countDiff n xs xs'
                  | x /= x'          = countDiff (n + 1) xs xs'
                  | otherwise        = -1
   where
    (x , xs ) = (head a, tail a)
    (x', xs') = (head b, tail b)

intersection :: String -> String -> String
intersection = doIntersection ""
 where
  doIntersection ret a b | null a || null b = ret
                         | x == x'          = doIntersection (ret ++ [x]) xs xs'
                         | x /= x'          = doIntersection ret xs xs'
   where
    (x , xs ) = (head a, tail a)
    (x', xs') = (head b, tail b)

takeFirst :: (a -> Bool) -> [a] -> Maybe a
takeFirst f a  = case filter f a of 
                   (x:_) -> Just (head (filter f a))
                   [] -> Nothing

firstIntersectionN :: Int -> [String] -> String -> Maybe String
firstIntersectionN n pool candidate = takeFirst (not . null) intersections
 where
  intersections = map (differsByN n candidate) pool
  differsByN n a b
    | lettersDiffering a b == n = intersection a b
    | otherwise = ""

findOffByN :: Int -> [String] -> [String] -> Maybe String
findOffByN n a a' = Maybe.fromMaybe Nothing ret
  where validIntersections = map (firstIntersectionN n a') a
        ret = takeFirst (not . null) validIntersections

printFirstOffsetId :: IO ()
printFirstOffsetId = do
  idData <- readFile "/home/nihliphobe/projects/haskell/aoc2018/data/day2.txt"
  case parse pIds "(unknown)" idData of
    Left  err -> fail (show err)
    Right ids -> print (findOffByN 1 ids ids)

printCheckSum :: IO ()
printCheckSum = do
  idData <- readFile "/home/nihliphobe/projects/haskell/aoc2018/data/day2.txt"
  case parse pIds "(unknown)" idData of
    Left  err -> fail (show err)
    Right ids -> print $ threeCount * fourCount
      where (threeCount, fourCount) = genChecksum ids
