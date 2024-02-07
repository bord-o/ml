run: build
	chmod +x ./_build/main && cd ./_build && ./main
build:
	mlton -output ./_build/main ./src/main.mlb

buildopt:
	mlton -cc-opt -O3 -output ./_build/mainopt ./src/main.mlb
format: 
	smlfmt --force ./src/ast.sml
	smlfmt --force ./src/interp.sml
	smlfmt --force ./src/repl.sml
	smlfmt --force ./src/main.sml
	smlfmt --force ./src/util.sml
lexparse:
	mllex ./src/main.lex
	mlyacc ./src/main.grm
