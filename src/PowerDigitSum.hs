{-# LANGUAGE BangPatterns #-}

module PowerDigitSum (
    digitSumTail,
    digitSumRecursive,
    variants,
) where

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

variants :: [(String, Integer -> Integer)]
variants =
    [ ("tail", digitSumTail),
    , ("recursive", digitSumRecursive)
    ]
