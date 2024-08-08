help:
	cat @Makefile

STACK=stack
GHCID=ghcid

build:
	$(STACK) build --ghc-options=-O2

develop:
	$(GHCID) --command "stack ghci" --restart stack.yaml

run:
	$(STACK) exec -- discordant $(ARGS)

watch:
	$(STACK) test --fast --file-watch

b: build
d: develop
r: run
w: watch
