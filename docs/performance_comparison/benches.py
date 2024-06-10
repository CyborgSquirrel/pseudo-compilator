import abc
import functools
import itertools
import math
import typing
from pathlib import Path
from random import Random

from runners import Runner

FILE = Path(__file__)
BENCH_DIR = FILE.parent / "bench"
RESULTS_PATH = FILE.parent / "results.csv"


class Bench(abc.ABC):
    @property
    @abc.abstractclassmethod
    def name(cls) -> str:
        pass

    @abc.abstractclassmethod
    def _gen_data(cls) -> str:
        pass

    @classmethod
    @functools.cache
    def data(cls) -> str:
        return cls._gen_data()

    @abc.abstractclassmethod
    def check(cls, output: str):
        pass

    @classmethod
    def run(cls, runner: typing.Type[Runner]):
        source_path = BENCH_DIR / cls.name / runner.file_name
        result = runner.run(source_path, cls.data())
        cls.check(result.stdout)
        return result


class BubbleSortBench(Bench):
    @classmethod
    @property
    def name(cls):
        return "bubble_sort"

    @classmethod
    @functools.cache
    def _sorted_list(cls):
        list_len = 5000
        list_ = list(range(list_len))
        return list_
    
    @classmethod
    @functools.cache
    def _shuffled_list(cls):
        rng = Random(41)
        list_ = cls._sorted_list().copy()
        rng.shuffle(list_)

        return list_
    
    @classmethod
    def _gen_data(cls):
        list_ = cls._shuffled_list()
        list_ = [len(list_)] + list_
        list_ = map(lambda a: str(a), list_)
        list_ = "\n".join(list_)
        list_ = list_.encode()
        return list_

    @classmethod
    def check(cls, output: str):
        output = output.split("\n")
        output = (line.strip() for line in output)
        output = (float(line) for line in output if line != "")
        output = list(output)

        for a, b in zip(cls._sorted_list(), output):
            assert math.isclose(a, b)

            
class Print1mBench(Bench):
    @classmethod
    @property
    def name(cls):
        return "print_1m"

    @classmethod
    def _gen_data(cls):
        return b""

    @classmethod
    @functools.cache
    def _expected_output(cls):
        return "Hello, World!\n" * 1000000

    @classmethod
    def check(cls, output: str):
        assert output.strip() == cls._expected_output().strip()


class ComputeDivisorsBench(Bench):
    @classmethod
    @property
    def name(cls):
        return "compute_divisors"

    @classmethod
    @property
    @functools.cache
    def _numbers(cls) -> list[int]:
        rng = Random(41)
        numbers = [rng.randint(10_000_000, 100_000_000) for _ in range(1000)]
        return numbers
    
    @classmethod
    def _gen_data(cls):
        return "\n".join(str(number) for number in [len(cls._numbers), *cls._numbers]).encode()

    @classmethod
    @functools.cache
    def _compute_factors(cls, a: int) -> list[int]:
        factors = []
        i = 1
        while i*i <= a:
            if a % i == 0:
                factors.append(i)
                if i != a:
                    factors.append(a//i)
            i += 1
        return factors

    @classmethod
    def check(cls, output: str):
        factors = itertools.chain.from_iterable(
            cls._compute_factors(number) for number in cls._numbers
        )
        
        output = output.split("\n")
        output = (line.strip() for line in output)
        output = (float(line) for line in output if line != "")
        output = list(output)

        for a, b in zip(factors, output):
            assert math.isclose(a, b)

            
class SieveOfEratosthenesBench(Bench):
    @classmethod
    @property
    def name(cls):
        return "sieve_of_eratosthenes"

    @classmethod
    @property
    def _number(cls):
        return 1_000_000
    
    @classmethod
    def _gen_data(cls):
        return str(cls._number).encode()

    @classmethod
    @functools.cache
    def _compute_primes(cls, a: int) -> list[int]:
        sieve = [True] * a
        primes = []

        for i in range(2, a):
            if sieve[i]:
                primes.append(i)
                for j in range(i+i, a, i):
                    sieve[j] = False

        return primes

    @classmethod
    def check(cls, output: str):
        primes = cls._compute_primes(cls._number)
        
        output = output.split("\n")
        output = (line.strip() for line in output)
        output = (float(line) for line in output if line != "")
        output = list(output)

        for a, b in zip(primes, output):
            assert math.isclose(a, b)

            
class ReverseListBench(Bench):
    @classmethod
    @property
    def name(cls):
        return "reverse_list"

    @classmethod
    @property
    def _list_len(cls):
        return 1_000_000
    
    @classmethod
    def _gen_data(cls):
        return "\n".join(str(number) for number in [len(cls._list), *cls._list]).encode()

    @classmethod
    @property
    @functools.cache
    def _list(cls):
        rng = Random(41)
        list_ = [rng.randint(1, 1000) for _ in range(cls._list_len)]
        return list_

    @classmethod
    def check(cls, output: str):
        reversed_ = reversed(cls._list)
        
        output = output.split("\n")
        output = (line.strip() for line in output)
        output = (float(line) for line in output if line != "")
        output = list(output)

        for a, b in zip(reversed_, output):
            assert math.isclose(a, b)


BENCHES = [
    BubbleSortBench,
    Print1mBench,
    ComputeDivisorsBench,
    SieveOfEratosthenesBench,
    ReverseListBench,
]
