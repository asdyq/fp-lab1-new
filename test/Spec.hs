module Main (main) where

import Control.Monad (unless)
import Data.Char (digitToInt)
import qualified PowerDigitSum as P16
import qualified SmallestMultiple as P05
import System.Exit (exitFailure)

type Check = (String, Bool)

main :: IO ()
main = do
    let failed = [name | (name, passed) <- checks, not passed]
    mapM_ (putStrLn . ("FAIL: " ++)) failed
    putStrLn (show (length checks - length failed) ++ "/" ++ show (length checks) ++ " checks passed")
    unless (null failed) exitFailure

checks :: [Check]
checks = known05 ++ brute05 ++ known16 ++ doubling16 ++ string16

-- Задача 5: известные ответы
known05 :: [Check]
known05 =
    [ ("5/" ++ name ++ "/" ++ show n, solve n == expected)
    | (name, solve) <- P05.variants
    , (n, expected) <- [(0, 1), (1, 1), (2, 2), (10, 2520), (20, 232792560), (40, 5342931457063200)]
    ]

-- Задача 5: сверка перебором кандидатов
brute05 :: [Check]
brute05 =
    [ ("5/brute/" ++ name ++ "/" ++ show n, solve n == smallest n)
    | (name, solve) <- P05.variants
    , n <- [1 .. 12]
    ]
  where
    smallest :: Integer -> Integer
    smallest n = head [m | m <- [1 ..], all (\d -> m `mod` d == 0) [1 .. n]]

-- Задача 16: известные ответы
known16 :: [Check]
known16 =
    [ ("16/" ++ name ++ "/" ++ show n, solve n == expected)
    | (name, solve) <- P16.variants
    , (n, expected) <- [(0, 1), (1, 2), (15, 26), (1000, 1366)]
    ]

-- Задача 16: сверка с суммой цифр, где степень двойки считается удвоением
doubling16 :: [Check]
doubling16 =
    [ ("16/doubling/" ++ name ++ "/" ++ show n, solve n == digitSum (powers !! fromInteger n))
    | (name, solve) <- P16.variants
    , n <- [0 .. 300]
    ]
  where
    powers = iterate (* 2) 1
    digitSum :: Integer -> Integer
    digitSum 0 = 0
    digitSum m = m `mod` 10 + digitSum (m `div` 10)

-- Независимая от арифметического извлечения цифр проверка
string16 :: [Check]
string16 =
    [ ("16/string/" ++ name ++ "/" ++ show n, solve n == expected n)
    | (name, solve) <- P16.variants
    , n <- [0 .. 300]
    ]
  where
    expected n = sum (map (toInteger . digitToInt) (show (2 ^ n :: Integer)))
