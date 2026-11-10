"""Задачи Эйлера 5 и 16 обычными циклами — для сравнения с Haskell."""

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


if __name__ == "__main__":
    print(smallest_multiple(20))
    print(power_digit_sum(1000))
