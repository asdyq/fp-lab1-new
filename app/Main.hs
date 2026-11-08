module Main (main) where

import qualified PowerDigitSum as P16
import qualified SmallestMultiple as P05

main :: IO ()
main = do
    mapM_ (\(name, solve) -> putStrLn ("5 " ++ name ++ " " ++ show (solve 20))) P05.variants
    mapM_ (\(name, solve) -> putStrLn ("16 " ++ name ++ " " ++ show (solve 1000))) P16.variants
