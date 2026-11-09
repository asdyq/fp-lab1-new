{-# LANGUAGE BangPatterns #-}

-- | Project Euler 5: НОК чисел от 1 до n (n >= 0; при n = 0 ответ 1).
module SmallestMultiple (
    lcmTail,
    lcmRecursive,
    lcmModular,
    lcmMap,
    lcmComprehension,
    lcmInfinite,
    variants,
) where

import Data.List (foldl', genericIndex)

-- Простое ли число (перебор делителей до корня)
isPrime :: Integer -> Bool
isPrime p = all (\d -> p `mod` d /= 0) (takeWhile (\d -> d * d <= p) [2 ..])

-- Наибольшая степень простого p, не превосходящая n
largestPower :: Integer -> Integer -> Integer
largestPower n p = last (takeWhile (<= n) (iterate (* p) p))

-- | Хвостовая рекурсия с аккумулятором
lcmTail :: Integer -> Integer
lcmTail n = go 1 1
  where
    go !i !acc
        | i > n = acc
        | otherwise = go (i + 1) (lcm acc i)

-- | Обычная рекурсия
lcmRecursive :: Integer -> Integer
lcmRecursive n
    | n <= 1 = 1
    | otherwise = lcm n (lcmRecursive (n - 1))

-- | Генерация, фильтрация, свёртка: числа из нижней половины делят какое-то число из верхней, поэтому их можно отбросить
lcmModular :: Integer -> Integer
lcmModular n = foldl' lcm 1 (filter (> n `div` 2) [1 .. n])

-- | Отображение map: наибольшие степени простых чисел до n, перемноженные
lcmMap :: Integer -> Integer
lcmMap n = product (map (largestPower n) (filter isPrime [2 .. n]))

-- | Списковое включение: произведение наибольших степеней простых чисел
lcmComprehension :: Integer -> Integer
lcmComprehension n = product [largestPower n p | p <- [2 .. n], isPrime p]

-- | Бесконечный список накопленных НОК: 1, lcm 1 1, lcm 1 2, ...
lcmInfinite :: Integer -> Integer
lcmInfinite = genericIndex (scanl lcm 1 [1 ..])

variants :: [(String, Integer -> Integer)]
variants =
    [ ("tail", lcmTail)
    , ("recursive", lcmRecursive)
    , ("modular", lcmModular)
    , ("map", lcmMap)
    , ("comprehension", lcmComprehension)
    , ("infinite", lcmInfinite)
    ]
