squall: ppx sample

foo: ppx foo.ml
	ocamlfind ppx_tools/rewriter ./ppx_toMeta.native foo.ml > temp.ml
	#cat temp.ml
	metaocamlc -dsource temp.ml
	rm *.cmi *.cmo *.out

ppx: ppx_toMeta.ml
	ocamlbuild -package compiler-libs.common ppx_toMeta.native

test: ppx test_toMeta.ml
	ocamlfind ppx_tools/rewriter ./ppx_toMeta.native test_toMeta.ml > test_res.ml
	#uncomment below lines to prettyprint result
	metaocamlc -dsource test_res.ml
	rm *.cmi *.cmo *.out

ast:
	ocamlfind ppx_tools/dumpast foo.ml

tAst:
	metaocamlc -dparsetree bar.ml
	rm *.cmi *.cmo *.out

cAst: ppx
	metaocamlc -dparsetree -ppx ./ppx_toMeta.native foo.ml
	rm *.cmi *.cmo *.out

clean:
	rm *.cmi *.cmo *~ *.out *.orig temp.ml test_res.ml

