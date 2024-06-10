.PHONY: coverage

coverage:
	python -m test-compiler
	llvm-profdata merge -sparse test-compiler/prof/*.profraw -o test-compiler/prof/merged.profdata
	llvm-cov report \
		-Xdemangler rustfilt \
		--use-color \
		--ignore-filename-regex='/.cargo/registry|pseudo-sys/' \
		--instr-profile=test-compiler/prof/merged.profdata \
		--object target/debug/pseudo-cli \
		--object target/debug/deps/pseudo_core-cb689f32d3b5fd44
