run: build
	chmod +x ./_build/main && cd ./_build && ./main

repl : all 
	chmod +x ./_build/main && cd ./_build && ./main

all: clean lexparse build

build:
	mkdir -p ./_build/
	mlton -output ./_build/main ./src/main.mlb

format: 
	smlfmt --force ./src/ast.sml
	smlfmt --force ./src/interp.sml
	smlfmt --force ./src/interp_tests.sml
	smlfmt --force ./src/repl.sml
	smlfmt --force ./src/main.sml
	smlfmt --force ./src/util.sml

lexparse:
	mllex ./src/main.lex
	mlyacc ./src/main.grm

clean:
	rm -f ./src/main.lex.sml
	rm -f ./src/main.grm.desc
	rm -f ./src/main.grm.sig
	rm -f ./src/main.grm.sml
	rm -rf ./_build

