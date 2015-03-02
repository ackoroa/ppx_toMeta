clean:
	rm *.cmo *.cmi *.cma

builder:
	ocamlbuild -r -package compiler-libs.common -package ppx_tools ppx_builder.native

builder-test:
	ocamlc -dsource -ppx ./ppx_builder.native test_builder.ml


ifdef:
	ocamlbuild -r -package compiler-libs.common -package ppx_tools ifdef.native
