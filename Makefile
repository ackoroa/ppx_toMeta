squall: ppx sample

sample: ppx foo.ml
	ocamlfind ppx_tools/rewriter ./ppx_toMeta.native foo.ml > temp.ml
	metaocamlc -dsource temp.ml
	rm *.cmi *.cmo *.out temp.ml

ppx: ppx_toMeta.ml
	ocamlbuild -package compiler-libs.common ppx_toMeta.native

test: ppx test_toMeta.ml
	ocamlfind ppx_tools/rewriter ./ppx_toMeta.native test_toMeta.ml > test_res.ml

ast:
	ocamlfind ppx_tools/dumpast foo.ml

tAst:
	ocamlfind ppx_tools/dumpast bar.ml

cAst: ppx
	ocamlfind ppx_tools/dumpast -ppx ./ppx_toMeta.native foo.ml

clean:
	rm *.cmi *.cmo *~ *.out

