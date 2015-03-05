clean:
	rm *.cmo *.cmi *.cma

builder:
	ocamlbuild -r -package compiler-libs.common -package ppx_tools ppx_builder.native

builder-t:
	ocamlc -dsource -ppx ./ppx_builder.native test_builder.ml


ifdef:
	ocamlbuild -r -package compiler-libs.common -package ppx_tools ifdef.native

eval:
	ocamlbuild -r -package compiler-libs.common -package ppx_tools eval.native

js:
	ocamlbuild -r -package compiler-libs.common -package ppx_tools js_syntax.native

matches:
	ocamlbuild -r -package compiler-libs.common -package ppx_tools ppx_matches.native

matches-t:
	ocamlc -dsource -ppx ./ppx_matches.native test_matches.ml

minidoc:
	ocamlbuild -r -package compiler-libs.common -package ppx_tools minidoc.native

nomli:
	ocamlbuild -r -package compiler-libs.common -package ppx_tools nomli.native

copy_td:
	ocamlbuild -r -package compiler-libs.common -package ppx_tools copy_typedef.native

unused:
	ocamlbuild -r -package compiler-libs.common -package ppx_tools unused_exported_values.native
