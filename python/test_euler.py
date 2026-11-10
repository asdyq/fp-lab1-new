import subprocess
import unittest
from pathlib import Path

from euler import power_digit_sum, smallest_multiple


class EulerTest(unittest.TestCase):
    def test_problem_5(self):
        for n, expected in [(0, 1), (10, 2520), (20, 232792560)]:
            with self.subTest(n=n):
                self.assertEqual(smallest_multiple(n), expected)

    def test_problem_16(self):
        for n, expected in [(0, 1), (15, 26), (1000, 1366)]:
            with self.subTest(n=n):
                self.assertEqual(power_digit_sum(n), expected)

    def test_program_output(self):
        binary = Path(__file__).resolve().parent.parent / "build" / "lab1"
        output = subprocess.check_output([binary], text=True).splitlines()
        names = ["tail", "recursive", "modular", "map", "comprehension", "infinite"]
        expected = [f"5 {name} {smallest_multiple(20)}" for name in names]
        expected += [f"16 {name} {power_digit_sum(1000)}" for name in names]
        self.assertEqual(output, expected)


if __name__ == "__main__":
    unittest.main()
