ld53.cabal: package.yaml
	test $$IN_NIX_SHELL
	hpack

run: ld53.cabal
	test $$IN_NIX_SHELL
	cabal run
.PHONY: run

repl: ld53.cabal
	test $$IN_NIX_SHELL
	cabal repl
.PHONY: repl
