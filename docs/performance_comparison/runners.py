import abc
import dataclasses
import re
import subprocess
import tempfile
from pathlib import Path


FILE = Path(__file__)
COMPILE_PY = FILE.parent.parent.parent / "pseudo" / "compile.py"

RE_USER_CPU_TIME = re.compile("User time \\(seconds\\): ([\\d\\.]+)")
RE_SYS_CPU_TIME = re.compile("System time \\(seconds\\): ([\\d\\.]+)")
RE_RESIDENT_SET_SIZE = re.compile("Maximum resident set size \\(kbytes\\): ([\\d\\.]+)")


def bench_process(*args: str, input: str) -> tuple[float, int, subprocess.CompletedProcess]:
    try:
        output = subprocess.run([
            "time", "-v",
            *args,
        ], input=input, check=True, capture_output=True)
    except subprocess.CalledProcessError as ex:
        print(ex.stderr)
        raise

    stderr = output.stderr.decode()
    user_cpu_time_s = float(RE_USER_CPU_TIME.search(stderr).group(1))
    sys_cpu_time_s = float(RE_SYS_CPU_TIME.search(stderr).group(1))
    resident_set_size_kb = int(RE_RESIDENT_SET_SIZE.search(stderr).group(1))
    cpu_time_s = user_cpu_time_s + sys_cpu_time_s

    return cpu_time_s, resident_set_size_kb, output


@dataclasses.dataclass
class RunResult:
    cpu_time_s: float
    resident_set_size_kb: float
    stdout: str


class Runner(abc.ABC):
    @property
    @abc.abstractclassmethod
    def name(cls) -> str:
        pass

    @property
    @abc.abstractclassmethod
    def file_name(cls) -> str:
        pass
    
    @abc.abstractclassmethod
    def run(source_path: Path, input: str) -> RunResult:
        pass


class PseudocodeRunner(Runner):
    @classmethod
    @property
    def name(cls) -> str:
        return "Pseudocode"

    @classmethod
    @property
    def file_name(cls) -> str:
        return "code.pseudo"

    def run(source_path: Path, input: str) -> RunResult:
        with tempfile.TemporaryDirectory() as tmp_dir:
            tmp_dir = Path(tmp_dir)
            exe_path = tmp_dir / "executable"

            subprocess.run([
                "python",
                str(COMPILE_PY),
                "--executable",
                "--opt", "default",
                str(source_path),
                str(exe_path),
            ], check=True)

            cpu_time_s, resident_set_size_kb, output = bench_process(
                str(exe_path),
                input=input,
            )
        return RunResult(
            cpu_time_s=cpu_time_s,
            resident_set_size_kb=resident_set_size_kb,
            stdout=output.stdout.decode(),
        )


class CRunner(Runner):
    @classmethod
    @property
    def name(cls) -> str:
        return "C"

    @classmethod
    @property
    def file_name(cls) -> str:
        return "code.c"

    def run(source_path: Path, input: str) -> RunResult:
        with tempfile.TemporaryDirectory() as tmp_dir:
            tmp_dir = Path(tmp_dir)
            exe_path = tmp_dir / "executable"
            subprocess.run([
                "clang",
                str(source_path),
                "-o", str(exe_path),
                "-O2",
            ], check=True)

            cpu_time_s, resident_set_size_kb, output = bench_process(
                str(exe_path),
                input=input,
            )
        return RunResult(
            cpu_time_s=cpu_time_s,
            resident_set_size_kb=resident_set_size_kb,
            stdout=output.stdout.decode(),
        )


class PythonRunner(Runner):
    @classmethod
    @property
    def name(cls) -> str:
        return "Python"

    @classmethod
    @property
    def file_name(cls) -> str:
        return "code.py"

    def run(source_path: Path, input: str) -> RunResult:
        cpu_time_s, resident_set_size_kb, output = bench_process(
            "python",
            str(source_path),
            input=input,
        )

        return RunResult(
            cpu_time_s=cpu_time_s,
            resident_set_size_kb=resident_set_size_kb,
            stdout=output.stdout.decode(),
        )


RUNNERS = [
    PseudocodeRunner,
    CRunner,
    PythonRunner,
]
