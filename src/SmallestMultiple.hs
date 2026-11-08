{-# LANGUAGE BangPatterns #-}

module SmallestMultiple (
    lcmTail,
    lcmRecursive,
    variants,
) where

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

variants :: [(String, Integer -> Integer)]
variants =
    [ ("tail", lcmTail),
    , ("recursive", lcmRecursive)
    ]
