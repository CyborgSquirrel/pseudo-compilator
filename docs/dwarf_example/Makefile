PWD = $(shell pwd)

all: target/main target/dwarf target/dwarf-processed

target/main: main.c
	mkdir -p target
	clang -g main.c -o target/main
	
target/dwarf: target/main
	mkdir -p target
	llvm-dwarfdump target/main > target/dwarf

target/dwarf-processed: target/dwarf
	rg '$(PWD)' -r '.' -N --passthru target/dwarf > target/dwarf-processed
