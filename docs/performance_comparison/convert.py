import csv
import itertools
import json
from pathlib import Path

FILE = Path(__file__)
RESULTS_CSV = FILE.parent / "results.csv"
RESULTS_JSON = FILE.parent / "results.json"

results = dict()

with RESULTS_CSV.open() as f:
    f = csv.reader(f)
    next(f)  # skip header

    f = sorted(list(f))
    for group, values in itertools.groupby(f, key=lambda a: a[0:2]):
        values = list(values)
        
        cpu_times_s = list(float(value[2]) for value in values)
        mean_cpu_time_s = sum(cpu_times_s) / len(cpu_times_s)

        resident_set_sizes_kb = list(int(value[3]) for value in values)
        mean_resident_set_size_kb = sum(resident_set_sizes_kb) / len(resident_set_sizes_kb)

        result = results.setdefault(group[0], dict()).setdefault(group[1], dict())
        result["cpu_time_s"] = mean_cpu_time_s
        result["resident_set_size_kb"] = mean_resident_set_size_kb

with RESULTS_JSON.open("w") as f:
    json.dump(results, f)
