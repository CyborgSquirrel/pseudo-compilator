import subprocess
import tempfile
import textwrap
import unittest
from pathlib import Path

FILE = Path(__file__)
CARGO_WORKSPACE = FILE.parent.parent
LIB_PATH = CARGO_WORKSPACE / "target" / "debug" / "libpseudo_sys.so"


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

			subprocess.run([
				"cargo", "run", "-p", "pseudo-cli",
				"--",
				"--lib-path", str(LIB_PATH),
				"--executable",
				str(src_path), str(exe_path),
			], capture_output=True)

			output = subprocess.run([
				str(exe_path),
			], capture_output=True)

			actual_output = output.stdout.decode()
			self.assertEqual(expected_output, actual_output)

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


if __name__ == "__main__":
	unittest.main()
