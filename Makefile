GHC ?= ghc

.PHONY: build run clean

build:
	mkdir -p build
	$(GHC) -isrc app/Main.hs -o build/lab1

run: build
	build/lab1

clean:
	rm -rf build
