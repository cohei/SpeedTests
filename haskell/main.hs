{-# LANGUAGE BangPatterns #-}

module Main (main) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as I (fromList, (!))
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as V (filter, generate, mapM_, unsafeIndex)

main :: IO ()
main = do
  let
    cached =
      -- id
      -- listCached
      -- mapCached
      vectorCached
    !f = cached powerOfItself
  -- mapM_ print $ filter (isMunchausen f) [0 .. 440000000]
  V.mapM_ print $ V.filter (isMunchausen f) $ V.generate 440000000 id

isMunchausen :: (Int -> Int) -> Int -> Bool
isMunchausen powerOfItselfCached n = go 0 n
  where
    go :: Int -> Int -> Bool
    go acc m
      | acc > n = False
      | m > 0 =
          let (rest, onesPlace) = quotRem m 10 in go (acc + powerOfItselfCached onesPlace) rest
      | otherwise = n == acc

powerOfItself :: Int -> Int
powerOfItself 0 = 0
powerOfItself i = i ^ i

listCached :: (Int -> Int) -> Int -> Int
listCached f = (cache !!)
  where
    cache :: [Int]
    cache = map f [0 .. 9]

mapCached :: (Int -> Int) -> Int -> Int
mapCached f = (cache I.!)
  where
    cache :: IntMap Int
    !cache = I.fromList $ map (\i -> (i, f i)) [0 .. 9]

vectorCached :: (Int -> Int) -> Int -> Int
vectorCached f = V.unsafeIndex cache
  where
    cache :: Vector Int
    !cache = V.generate 10 f
