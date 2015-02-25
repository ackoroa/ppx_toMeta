all: ppx sample

sample: ppx foo.ml
	ocamlfind ppx_tools/rewriter ./ppx_toMeta.native foo.ml

ppx: ppx_toMeta.ml
	ocamlbuild -package compiler-libs.common ppx_toMeta.native

ast: foo.ml
	ocamlfind ppx_tools/dumpast foo.ml

tAst: bar.ml
	ocamlfind ppx_tools/dumpast bar.ml

cAst: ppx
	ocamlfind ppx_tools/dumpast -ppx ./ppx_toMeta.native foo.ml

clean:
	rm *.cmi *.cmo

