{-# LANGUAGE BangPatterns #-}

-- | Project Euler 16: сумма цифр числа 2^n (n >= 0; при n = 0 ответ 1).
module PowerDigitSum (
    digitSumTail,
    digitSumRecursive,
    digitSumModular,
    digitSumMap,
    digitSumComprehension,
    digitSumInfinite,
    variants,
) where

import Data.Char (digitToInt)
import Data.List (foldl')

-- 2^n как Integer
powerOfTwo :: Integer -> Integer
powerOfTwo n = 2 ^ n

-- | Хвостовая рекурсия с аккумулятором
digitSumTail :: Integer -> Integer
digitSumTail n = go (powerOfTwo n) 0
  where
    go !m !acc
        | m == 0 = acc
        | otherwise = go (m `div` 10) (acc + m `mod` 10)

-- | Обычная рекурсия
digitSumRecursive :: Integer -> Integer
digitSumRecursive n = digitSum (powerOfTwo n)
  where
    digitSum m
        | m == 0 = 0
        | otherwise = m `mod` 10 + digitSum (m `div` 10)

-- | Генерация, фильтрация, свёртка: нули ничего не добавляют, их отбрасываем
digitSumModular :: Integer -> Integer
digitSumModular n = foldl' (+) 0 (filter (/= 0) (map (toInteger . digitToInt) (show (powerOfTwo n))))

-- | Отображение map
digitSumMap :: Integer -> Integer
digitSumMap n = sum (map (toInteger . digitToInt) (show (powerOfTwo n)))

-- | Списковое включение
digitSumComprehension :: Integer -> Integer
digitSumComprehension n = sum [toInteger (digitToInt c) | c <- show (powerOfTwo n)]

-- | Бесконечный список: 2^n, 2^n div 10, 2^n div 100, ... до нуля
digitSumInfinite :: Integer -> Integer
digitSumInfinite n = sum (map (`mod` 10) (takeWhile (> 0) (iterate (`div` 10) (powerOfTwo n))))

variants :: [(String, Integer -> Integer)]
variants =
    [ ("tail", digitSumTail)
    , ("recursive", digitSumRecursive)
    , ("modular", digitSumModular)
    , ("map", digitSumMap)
    , ("comprehension", digitSumComprehension)
    , ("infinite", digitSumInfinite)
    ]
