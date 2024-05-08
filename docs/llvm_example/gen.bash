#!/usr/bin/env bash

rm -rf target
mkdir -p target
clang main.c -o target/main
clang main.c -S -emit-llvm -o target/main.ll
clang --version
clang --version | rg -o 'clang version (.+)' -r '$1' > target/clang_version.txt
