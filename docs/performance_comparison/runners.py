import abc
import dataclasses
import re
import subprocess
import tempfile
from pathlib import Path


FILE = Path(__file__)
COMPILE_PY = FILE.parent.parent.parent / "pseudo" / "compile.py"

RE_USER_CPU_TIME = re.compile("user ([\\d\\.]+)")
RE_SYS_CPU_TIME = re.compile("sys ([\\d\\.]+)")


def time_process(*args: str, input: str) -> tuple[float, subprocess.CompletedProcess]:
    try:
        output = subprocess.run([
            "time", "-p",
            *args,
        ], input=input, check=True, capture_output=True)
    except subprocess.CalledProcessError as ex:
        print(ex.stderr)
        raise

    stderr = output.stderr.decode()
    user_cpu_time = float(RE_USER_CPU_TIME.search(stderr).group(1))
    sys_cpu_time = float(RE_SYS_CPU_TIME.search(stderr).group(1))
    cpu_time = user_cpu_time + sys_cpu_time

    return cpu_time, output


@dataclasses.dataclass
class RunResult:
    cpu_time: float
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

            cpu_time, output = time_process(
                str(exe_path),
                input=input,
            )
        return RunResult(
            cpu_time=cpu_time,
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

            cpu_time, output = time_process(
                str(exe_path),
                input=input,
            )
        return RunResult(
            cpu_time=cpu_time,
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
        cpu_time, output = time_process(
            "python",
            str(source_path),
            input=input,
        )

        return RunResult(
            cpu_time=cpu_time,
            stdout=output.stdout.decode(),
        )


RUNNERS = [
    PseudocodeRunner,
    CRunner,
    PythonRunner,
]
