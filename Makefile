.PHONY: build
build:
	stack build

.PHONY: buildsplices
buildsplices:
	stack build --ghc-options='-ddump-splices'

.PHONY: dumpsplices
printsplices:
	find .stack-work -iname "*.dump-splices" | xargs -t -L1 cat

.PHONY: splices
splices: buildsplices printsplices

.PHONY: test
test:
	stack test

.PHONY: deps
deps:
	stack build apply-refact hlint hindent intero stylish-haskell ghcid

.PHONY: watch
watch:
	stack exec ghcid

.PHONY: format
format:
	find . -name '*.hs' | xargs -t stack exec stylish-haskell -- -i

.PHONY: lint
lint:
	stack exec hlint -- -i 'Parse error' -i 'Reduce duplication' -i 'Use newtype instead of data' src

.PHONY: refactor
refactor:
	find . -path ./.stack-work -prune -o -name '*.hs' -exec bash -c "echo {} && stack exec hlint -- --refactor --refactor-options -i {} || true" \;

.PHONY: indent
indent:
	find . -path ./.stack-work -prune -o -name '*.hs' -exec bash -c "echo {} && stack exec hindent {} || true" \;

.PHONY: repl
repl:
	stack exec stack build && stack exec simplytyped-exe
