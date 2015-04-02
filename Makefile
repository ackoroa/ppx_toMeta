PPX = ppx_toMeta1

squall: ppx sample

foo: ppx foo.ml
	ocamlfind ppx_tools/rewriter ./$(PPX).native foo.ml > temp.ml
	#cat temp.ml
	metaocamlc -dsource temp.ml
	rm *.cmi *.cmo *.out

intr: ppx interpreter.ml interpreter_defs.ml
	ocamlfind ppx_tools/rewriter ./$(PPX).native interpreter.ml > temp.ml
	metaocamlc interpreter_defs.ml	
	metaocamlc -dsource interpreter.ml

ppx: $(PPX).ml
	ocamlbuild -package compiler-libs.common $(PPX).native

test: ppx test_toMeta.ml
	ocamlfind ppx_tools/rewriter ./$(PPX).native test_toMeta.ml > test_res.ml
	#uncomment below lines to prettyprint result
	metaocamlc -dsource test_res.ml
	rm *.cmi *.cmo *.out

ast:
	ocamlfind ppx_tools/dumpast foo.ml

tAst:
	metaocamlc -dparsetree bar.ml
	rm *.cmi *.cmo *.out

cAst: ppx
	metaocamlc -dparsetree -ppx ./$(PPX).native foo.ml
	rm *.cmi *.cmo *.out

clean:
	rm *.cmi *.cmo *~ *.out *.orig temp.ml test_res.ml

