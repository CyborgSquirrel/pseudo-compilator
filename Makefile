PWD=$(shell pwd)

.PHONY: all

all: \
	editor-backend/proto/__init__.py \
	docs/bench_server/proto/__init__.py \
	editor-frontend/src/proto/main.js \
	editor-frontend/src/proto/main.d.ts

docs/bench_server/proto/__init__.py: main.proto
	mkdir -p docs/bench_server/proto
	protoc --python_betterproto_out=./docs/bench_server/proto main.proto

editor-backend/proto/__init__.py: main.proto
	mkdir -p editor-backend/proto
	protoc --python_betterproto_out=./editor-backend/proto main.proto

editor-frontend/src/proto/main.js: main.proto
	mkdir -p editor-frontend/src/proto

	cd editor-frontend/ \
	&& npx npx pbjs -t static-module -w es6 -o src/proto/main.js $(PWD)/main.proto

editor-frontend/src/proto/main.d.ts: editor-frontend/src/proto/main.js
	mkdir -p editor-frontend/src/proto

	cd editor-frontend/ \
	&& npx pbts src/proto/main.js -o src/proto/main.d.ts

pseudo/target/release/pseudo-cli:
	echo hi
