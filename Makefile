
BINARY := loco

.PHONY: build repl test format lines coverage clean help

build: $(BINARY) ## Build the module

repl: $(BINARY) ## Start REPL
	@ ./$(BINARY)

_build/default/main.exe: *.ml
	dune build

$(BINARY): _build/default/main.exe
	@ cp -f _build/default/main.exe $(BINARY)

test: clean build ## Run unit tests
	dune runtest
	cd examples/the-little-schemer/ && ../../loco run-all.scm

lines: ## Count the number of code lines
	@ find . -type f \( -name "*.ml" -not -path "./_build/*" -not -path "./test/*" -not -path "./.github/*" \) -exec cat {} \; | grep . | wc -l

coverage: clean ## Create test coverage report
	BISECT_ENABLE=yes dune build
	dune runtest
	bisect-ppx-report html
	$(MAKE) --no-print-directory clean build

clean: ## Cleanup
	@ rm -rf _build
	@ rm -rf $(BINARY)

help: ## Display this help
	@ grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
