# Common tasks. Cabal targets run inside the dev shell when not already in
# one, so `make test` works from any terminal. Nix only sees files tracked by
# git: `git add` new files before `make release` or `make install`.

ifdef IN_NIX_SHELL
DEV :=
else
DEV := nix develop --command
endif

CONFIG ?= config/kineo.toml
QUICKCHECK ?= 5000

.PHONY: all build test test-long doctor check-config format dev release install run clean

all: build

build:
	$(DEV) cabal build

test:
	$(DEV) cabal test

test-long:
	$(DEV) cabal test --test-options='--quickcheck-tests=$(QUICKCHECK)'

# Read-only: permissions, displays, windows.
doctor:
	$(DEV) cabal run kineo -- doctor

check-config:
	$(DEV) cabal run kineo -- check-config $(CONFIG)

format:
	$(DEV) fourmolu --mode inplace $$(git ls-files '*.hs')

# kineo and kineo-hyper from one terminal, debug logs on. Takes over window
# management.
dev:
	nix run .#dev

# Release binary; runs the tests too.
release:
	nix build

# Build, sign and start Kineo.app in ~/Applications.
install:
	nix run .#install

# Takes over window management and rearranges every window on screen.
run:
	nix run .#kineo

clean:
	rm -rf dist-newstyle result result-*
