all: ppx sample

sample: ppx_toMeta.native foo.ml
	ocamlc -dsource -ppx ./ppx_toMeta.native foo.ml
	rm *.cmi *.cmo

ppx: ppx_toMeta.ml
	ocamlbuild -package compiler-libs.common ppx_toMeta.native

ast: foo.ml
	ocamlfind ppx_tools/dumpast foo.ml

clean:
	rm *.cmi *.cmo

