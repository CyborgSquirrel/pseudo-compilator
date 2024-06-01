import csv
from pathlib import Path

from benches import BENCHES
from runners import RUNNERS

FILE = Path(__file__)
BENCH_DIR = FILE.parent / "bench"
RESULTS_PATH = FILE.parent / "results.csv"


with RESULTS_PATH.open("w") as f:
    f = csv.writer(f)
    for bench in BENCHES:
        print(bench.name)
        for runner in RUNNERS:
            print(runner.name)
            cpu_times = []
            for _ in range(5):
                result = bench.run(runner)
                print(result.cpu_time)
                f.writerow((bench.name, runner.name, result.cpu_time))
