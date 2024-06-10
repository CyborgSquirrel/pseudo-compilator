import csv
from pathlib import Path

from benches import BENCHES
from runners import RUNNERS

FILE = Path(__file__)
BENCH_DIR = FILE.parent / "bench"
RESULTS_PATH = FILE.parent / "results.csv"


with RESULTS_PATH.open("w") as f:
    f = csv.writer(f)
    f.writerow(("bench", "runner", "cpu_time_s", "resident_set_size_kb"))
    for bench in BENCHES:
        print(bench.name)
        for runner in RUNNERS:
            print(runner.name)
            cpu_times = []
            for _ in range(5):
                result = bench.run(runner)
                print("cpu_time_s", result.cpu_time_s)
                print("resident_set_size_kb", result.resident_set_size_kb)
                f.writerow((bench.name, runner.name, result.cpu_time_s, result.resident_set_size_kb))
