"""
Convenience wrapper for compiler, that automatically finds libpseudo_sys.so.
"""

import subprocess
import sys
from pathlib import Path

FILE = Path(__file__)
CARGO_WORKSPACE = FILE.parent
DEBUG_PATH = CARGO_WORKSPACE / "target" / "debug"
LIB_PATH = DEBUG_PATH / "libpseudo_sys.so"
EXE_PATH = DEBUG_PATH / "pseudo-cli"

if not LIB_PATH.exists():
	subprocess.run([
		"cargo", "build", "-p", "pseudo-sys",
	], cwd=str(CARGO_WORKSPACE))

if not EXE_PATH.exists():
	subprocess.run([
		"cargo", "build", "-p", "pseudo-cli",
	], cwd=str(CARGO_WORKSPACE))

subprocess.run([
	str(EXE_PATH),
	"--lib-path", str(LIB_PATH),
	*sys.argv[1:],
])
