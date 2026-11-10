GHC ?= ghc
HLINT ?= hlint
FOURMOLU ?= fourmolu
FLAGS := -Wall -Werror -isrc
SOURCES := $(shell find src app test -name '*.hs')

.PHONY: build test lint fmt fmt-check run clean check

build: build/lab1

build/lab1: $(SOURCES)
	mkdir -p build/app
	$(GHC) $(FLAGS) -odir build/app -hidir build/app app/Main.hs -o $@

build/tests: $(SOURCES)
	mkdir -p build/test
	$(GHC) $(FLAGS) -odir build/test -hidir build/test test/Spec.hs -o $@

test: build/lab1 build/tests
	build/tests
	cd python && PYTHONDONTWRITEBYTECODE=1 python3 -m unittest -v

lint:
	$(HLINT) src app test

fmt:
	$(FOURMOLU) --mode inplace $(SOURCES)

fmt-check:
	$(FOURMOLU) --mode check $(SOURCES)

run: build/lab1
	build/lab1

clean:
	rm -rf build

check: fmt-check lint build test
