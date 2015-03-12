all: ppx sample

sample: ppx foo.ml
	ocamlfind ppx_tools/rewriter ./ppx_toMeta.native foo.ml

ppx: ppx_toMeta.ml
	ocamlbuild -package compiler-libs.common ppx_toMeta.native

pp: ToMetaPostProc.java
	javac ToMetaPostProc.java

test: ppx pp test_toMeta.ml
	ocamlfind ppx_tools/rewriter ./ppx_toMeta.native test_toMeta.ml > testRes.out
	java ToMetaPostProc testRes.out > testMeta.ml

ast:
	ocamlfind ppx_tools/dumpast foo.ml

tAst:
	ocamlfind ppx_tools/dumpast bar.ml

cAst: ppx
	ocamlfind ppx_tools/dumpast -ppx ./ppx_toMeta.native foo.ml

clean:
	rm *.cmi *.cmo *~ *.class *.out

