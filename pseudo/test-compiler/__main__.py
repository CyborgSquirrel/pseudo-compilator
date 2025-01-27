import collections
import itertools
import os
import shutil
import subprocess
import tempfile
import textwrap
import unittest
from pathlib import Path

FILE = Path(__file__)
CARGO_WORKSPACE = FILE.parent.parent
LIB_PATH = CARGO_WORKSPACE / "target" / "debug" / "libpseudo_sys.so"

LLVM_PROFILE_DIR = FILE.parent / "prof"
LLVM_PROFILE_MARKER = LLVM_PROFILE_DIR / "marker.txt"
LLVM_PROFILE_MARKER_TEXT = "This directory contains LLVM profiling stuff!"
if LLVM_PROFILE_DIR.exists():
	if LLVM_PROFILE_MARKER.read_text().strip() != LLVM_PROFILE_MARKER_TEXT:
		raise RuntimeError("Marker in profiling directory does not match")

	shutil.rmtree(LLVM_PROFILE_DIR)

LLVM_PROFILE_DIR.mkdir(parents=True)
LLVM_PROFILE_MARKER.write_text(LLVM_PROFILE_MARKER_TEXT)

LLVM_PROFILE_FILE = FILE.parent / "prof" / "default_%m_%p.profraw"

LLVM_EXEC_PROFILE_FILES = (FILE.parent / "prof" / f"exec_{i}_%m_%p.profraw" for i in itertools.count())


class Test(unittest.TestCase):
	def assertExpectedOutput(
		self,
		source: str,
		expected_output: str,
	):
		source = textwrap.dedent(source)
	
		with tempfile.TemporaryDirectory() as tmp_dir:
			tmp_dir = Path(tmp_dir)
			src_path = tmp_dir / "src"
			exe_path = tmp_dir / "exe"

			src_path.write_text(source)

			env = dict(
				RUSTFLAGS="-C instrument-coverage",
				LLVM_PROFILE_FILE=str(next(LLVM_EXEC_PROFILE_FILES)),
			)
			proc_env = collections.ChainMap(env, os.environ)

			subprocess.run(
                [
					"cargo", "run", "-p", "pseudo-cli", "--",
					"--lib-path", str(LIB_PATH),
					"--executable",
					str(src_path), str(exe_path),
				],
				env=proc_env,
				capture_output=True,
			)

			output = subprocess.run([
				str(exe_path),
			], capture_output=True, check=True)

			actual_output = output.stdout.decode()
			self.assertEqual(expected_output, actual_output)

	def test_other(self):
		env = dict(
			RUSTFLAGS="-C instrument-coverage",
			LLVM_PROFILE_FILE=str(LLVM_PROFILE_FILE),
		)
		proc_env = collections.ChainMap(env, os.environ)

		subprocess.run(
            [
				"cargo", "test", "--tests",
			],
			env=proc_env,
			check=True,
		)

	def test_pentru_executa(self):
		self.assertExpectedOutput(
			"""
			pentru i <- 1,10 executa
				scrie i
			""",
			"1.000000\n2.000000\n3.000000\n4.000000\n5.000000\n6.000000\n7.000000\n8.000000\n9.000000\n10.000000\n",
		)

	def test_pentru_executa_backwards(self):
		self.assertExpectedOutput(
	 		"""
 			pentru i <- 10,5,-1 executa
 				scrie i
	 		""",
	 		"10.000000\n9.000000\n8.000000\n7.000000\n6.000000\n5.000000\n",
		)

	def test_pentru_executa_once(self):
		self.assertExpectedOutput(
			"""
			pentru i <- 10,10 executa
				scrie i
			""",
			"10.000000\n",
		)

	def test_pentru_executa_nope(self):
		self.assertExpectedOutput(
			"""
			pentru i <- 20,10 executa
				scrie i
			""",
			"",
		)

	def test_pentru_executa_big_increment(self):
		self.assertExpectedOutput(
			"""
			pentru i <- 1,10,4 executa
				scrie i
			""",
			"1.000000\n5.000000\n9.000000\n",
		)

	def test_pentru_executa_big_decrement(self):
		self.assertExpectedOutput(
			"""
			pentru i <- 15,0,-4 executa
				scrie i
			""",
			"15.000000\n11.000000\n7.000000\n3.000000\n",
		)

	def test_scrie_arguments(self):
		self.assertExpectedOutput(
			"""
			a<-10
			scrie 1, ' ', 5+3+a, "", "very nice ",42, 'x'
			""",
			"1.000000 18.000000very nice 42.000000x\n",
		)

	def test_whole_part(self):
		self.assertExpectedOutput(
			"""
			pentru i <- -5,5 executa
				scrie [i/2]
			""",
			"-2.000000\n-2.000000\n-1.000000\n-1.000000\n0.000000\n0.000000\n0.000000\n1.000000\n1.000000\n2.000000\n2.000000\n",
		)

	def test_eq_epsilon(self):
		self.assertExpectedOutput(
			"""
			a <- 41.0000000001
			b <- 41.0000000002
			daca a = b atunci
				scrie "da"
			""",
			"da\n",
		)

	def test_insereaza_sterge(self):
		self.assertExpectedOutput(
			"""
			list <- ,
			pentru i<-1,10 execută
				inserează list,i-1,i
			pentru i<-0,lungime(list)-1 execută
				scrie list[i]
			cât timp lungime(list) > 0 execută
				șterge list,lungime(list)-1
			scrie lungime(list)
			""",
			"1.000000\n2.000000\n3.000000\n4.000000\n5.000000\n"
			"6.000000\n7.000000\n8.000000\n9.000000\n10.000000\n0.000000\n",
		)

	def test_stack_doesnt_overflow(self):
		"""Make sure the stack does not overflow, when program declares many variables. """
		
		self.assertExpectedOutput(
			"""
			pentru i<-0,1000 executa
				pentru j<-0,1000 executa
					x <- 1
			""",
			"",
		)


if __name__ == "__main__":
	unittest.main()
