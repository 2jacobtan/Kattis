{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Reduction where

import Control.Arrow ((>>>))
import Data.Functor ( (<&>) )
import Text.Parsec ( parse, letter, digit, many1, string, Parsec )
import Data.String (IsString (fromString))
import Control.Monad (void)
import Data.Function ( (&) )
import Data.List ( sortOn )
import Data.Tuple ( swap )
-- import Debug.Trace (trace)


{- helpers -}

(|>) :: (a -> b) -> (b -> c) -> (a -> c)
(|>) = (>>>)
infixr 1 |>

instance (a ~ String) => IsString (Parsec String () a) where
  fromString = string

digits :: Parsec String () String
digits =  many1 digit

letters :: Parsec String () String
letters = many1 letter

getParse :: (Parsec String () c -> String -> c)
getParse p s = either (error . ("Parsec: " ++) . show) id $ parse p "" s

main :: IO ()
main = interact solve

test :: IO ()
test = readFile testInputFile >>= (solve |> putStr)


{- solution -}

testInputFile :: FilePath
testInputFile = "reduction.txt"

solve :: String -> String
solve = concat . solve2 . lines

solve2 :: [String] -> [String]
solve2 = tail |> formatInput |> map solve3
  |> zipWith (\i case' -> "Case " ++ show @Int i ++ "\n" ++ case') [1..]

solve3 :: (Int, Int, [([Char], Int, Int)]) -> String
solve3 (n0, m, agencies) = unlines $
  map (solve4 n0 m) agencies & sortOn swap
  & map (\(name, cost) -> name ++ " " ++ show @Int cost)

solve4 :: Int -> Int -> (String, Int, Int) -> (String, Int)
solve4 n0 m (name, oneRate, halfRate) =
  -- trace (show (n0, m, name, oneRate, halfRate)) $
  (name, go 0 n0)
  where
    go !acc !n =
      let
        -- diff = n - m
        half_quantity = n `div` 2 :: Int
        oneByOne = (n - half_quantity) * oneRate
      in
        -- trace (show (name, acc, n)) $
        if oneByOne <= halfRate || half_quantity < m
          then acc + (n - m) * oneRate
          else go (acc + halfRate) half_quantity

formatInput :: [String] -> [(Int, Int, [([Char], Int, Int)])]
formatInput (row:rows) =
  let
    n:m:l:_ = words row <&> read @Int
    (agencies, rest) = splitAt l rows
    agenciesParser = do
      agencyName <- many1 letter
      void ":"
      oneRate <- digits <&> read @Int
      void ","
      halfRate <- digits <&> read @Int
      return (agencyName, oneRate, halfRate)
    agenciesParsed = agencies <&> getParse agenciesParser 
  in (n,m,agenciesParsed) : formatInput rest
formatInput [] = []