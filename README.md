# Лабораторная работа № 1. Функциональное программирование

Студент: Лучкин Т.Д.  
Группа: P3421  
Преподаватель: Пенской А.В.

Решение задач 5 и 16 проекта Эйлера на Haskell с использованием рекурсии, функций высшего порядка и ленивых списков. Для сравнения те же задачи решены на Python.

## Условия и результаты

**Задача 5.** Найти наименьшее положительное число, которое делится на все числа от 1 до 20. Например, для диапазона от 1 до 10 ответ — 2520. Для диапазона от 1 до 20 получено **232792560**.

**Задача 16.** Найти сумму цифр числа 2¹⁰⁰⁰. Например, 2¹⁵ = 32768, сумма его цифр равна 26. Для 2¹⁰⁰⁰ получено **1366**.

Обе задачи обобщены: функции принимают неотрицательное целое `n` и возвращают `Integer`. В задаче 5 при `n = 0` НОК пустого диапазона принят за 1; при `n = 1` результат тоже равен 1. В задаче 16 при `n = 0` результат равен 1, поскольку 2⁰ = 1. Отрицательные значения не входят в область определения.

## Задача 5. Наименьшее общее кратное

НОК диапазона можно найти последовательно: начиная с 1, добавлять по одному числу и вычислять новый `lcm`. Хвостовая рекурсия хранит накопленный результат, а в обычной рекурсии очередной `lcm` вычисляется после возврата из вызова. Строгие параметры с `!` не дают аккумулятору накапливать цепочку отложенных вычислений.

В модульном варианте создаётся диапазон, из него фильтром выбирается верхняя половина, затем находится её НОК. Нижнюю половину можно отбросить: для каждого её числа последовательное удвоение приводит к кратному в верхней половине. В вариантах с `map` и списковым включением перемножаются наибольшие степени простых чисел, не превосходящие `n`; это два выражения одного алгоритма. В бесконечном списке `scanl` строит накопленные НОК, а `genericIndex` выбирает нужный элемент.

Фрагменты из [src/SmallestMultiple.hs](src/SmallestMultiple.hs). В модуле включено расширение `BangPatterns` и импортированы `foldl'` и `genericIndex` из `Data.List`:

```haskell
lcmTail :: Integer -> Integer
lcmTail n = go 1 1
  where
    go !i !acc
        | i > n = acc
        | otherwise = go (i + 1) (lcm acc i)

lcmRecursive :: Integer -> Integer
lcmRecursive n
    | n <= 1 = 1
    | otherwise = lcm n (lcmRecursive (n - 1))

lcmModular :: Integer -> Integer
lcmModular n = foldl' lcm 1 (filter (> n `div` 2) [1 .. n])

isPrime :: Integer -> Bool
isPrime p = all (\d -> p `mod` d /= 0) (takeWhile (\d -> d * d <= p) [2 ..])

largestPower :: Integer -> Integer -> Integer
largestPower n p = last (takeWhile (<= n) (iterate (* p) p))

lcmMap :: Integer -> Integer
lcmMap n = product (map (largestPower n) (filter isPrime [2 .. n]))

lcmComprehension :: Integer -> Integer
lcmComprehension n = product [largestPower n p | p <- [2 .. n], isPrime p]

lcmInfinite :: Integer -> Integer
lcmInfinite = genericIndex (scanl lcm 1 [1 ..])
```

## Задача 16. Сумма цифр степени двойки

Сначала находится `2 ^ n`. В рекурсивных вариантах `mod 10` выделяет последнюю цифру, а `div 10` удаляет её. Хвостовая версия накапливает сумму, обычная складывает цифры при возврате из рекурсии.

Модульное решение переводит число в строку, преобразует символы в цифры, отбрасывает нули и складывает результат свёрткой. Нули отбрасываются для демонстрации фильтрации: на сумму они не влияют. В вариантах с `map` и списковым включением цифры складываются без этого фильтра. Бесконечная последовательность частных ограничивается при первом нуле; остатки от деления на 10 дают цифры числа.

Фрагменты из [src/PowerDigitSum.hs](src/PowerDigitSum.hs). Модуль включает `BangPatterns` и импортирует `digitToInt` из `Data.Char` и `foldl'` из `Data.List`:

```haskell
powerOfTwo :: Integer -> Integer
powerOfTwo n = 2 ^ n

digitSumTail :: Integer -> Integer
digitSumTail n = go (powerOfTwo n) 0
  where
    go !m !acc
        | m == 0 = acc
        | otherwise = go (m `div` 10) (acc + m `mod` 10)

digitSumRecursive :: Integer -> Integer
digitSumRecursive n = digitSum (powerOfTwo n)
  where
    digitSum m
        | m == 0 = 0
        | otherwise = m `mod` 10 + digitSum (m `div` 10)

digitSumModular :: Integer -> Integer
digitSumModular n = foldl' (+) 0 (filter (/= 0) (map (toInteger . digitToInt) (show (powerOfTwo n))))

digitSumMap :: Integer -> Integer
digitSumMap n = sum (map (toInteger . digitToInt) (show (powerOfTwo n)))

digitSumComprehension :: Integer -> Integer
digitSumComprehension n = sum [toInteger (digitToInt c) | c <- show (powerOfTwo n)]

digitSumInfinite :: Integer -> Integer
digitSumInfinite n = sum (map (`mod` 10) (takeWhile (> 0) (iterate (`div` 10) (powerOfTwo n))))
```

## Решение на Python

В [python/euler.py](python/euler.py) НОК находится циклом через наибольший общий делитель `gcd`; сумма цифр — циклом с делением и остатком:

```python
from math import gcd


def smallest_multiple(n: int) -> int:
    result = 1
    for i in range(1, n + 1):
        result = result * i // gcd(result, i)
    return result


def power_digit_sum(n: int) -> int:
    power = 2**n
    total = 0
    while power > 0:
        total += power % 10
        power //= 10
    return total

```

## Проверка и запуск

Haskell-тесты сравнивают каждый вариант с известными ответами. Для задачи 5 дополнительно используется прямой перебор кандидатов при `n` от 1 до 12. Для задачи 16 проверяются значения от 0 до 300: степень вычисляется удвоением, а цифры выделяются делением; отдельно проверяется сумма цифр строкового представления. Python-тесты проверяют обе функции и сопоставляют вывод Haskell-программы с решениями на Python.

Проверки: 3744 случая Haskell и три теста Python. GitHub Actions выполняет `make check` при отправке изменений и открытии pull request.

Для запуска из корня проекта нужны GHC 9.6, HLint, Fourmolu, `make` и Python 3:

```bash
make run    # результаты всех вариантов для 20 и 1000
make check  # проверка форматирования Haskell, HLint, сборка и тесты
```

Основная программа — [app/Main.hs](app/Main.hs), тесты — [test/Spec.hs](test/Spec.hs) и [python/test_euler.py](python/test_euler.py).

## Выводы

1. Для задачи 5 НОК диапазона от 1 до 20 равен 232792560. В модульном решении достаточно верхней половины диапазона: она содержит кратное каждого отброшенного числа. Тот же НОК получается как произведение наибольших допустимых степеней простых чисел.
2. Для задачи 16 сумма цифр 2¹⁰⁰⁰ равна 1366. Отбрасывание нулей в модульном решении показывает фильтрацию и не меняет сумму.
3. Строгий аккумулятор хвостовых решений не накапливает цепочку отложенных вычислений. `map` и списковое включение дают два способа записать преобразование, а ленивый список позволяет вычислить конечную часть бесконечной последовательности. Python-решения используют циклы и изменяемые переменные.
