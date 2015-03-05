ROOT=../..
OCAMLC=$(ROOT)/boot/ocamlrun $(ROOT)/ocamlc -I $(ROOT)/stdlib -I $(ROOT)/parsing -I $(ROOT)/utils -I $(ROOT)/tools -I $(ROOT)/ppx_tools -I $(ROOT)/typing -I $(ROOT)/driver -I $(ROOT)/toplevel -w A-4-9-42 -annot 
COMMON=$(ROOT)/compilerlibs/ocamlcommon.cma
BYTECMP=$(ROOT)/compilerlibs/ocamlbytecomp.cma
TOPLVL=$(ROOT)/compilerlibs/ocamltoplevel.cma
INCL=/home/pls2nus/hg/src/ocaml-4.02.1/tools
clean:
	rm -f *.exe *.cm* *~ *.native _build/*

## Detecting unused exported values

.PHONY: unused_exported_values
unused_exported_values:
	$(OCAMLC) -o unused_exported_values.exe $(COMMON) $(ROOT)/tools/tast_iter.cmo unused_exported_values.ml


## Conditional compilation based on environment variables

.PHONY: ifdef
ifdef:
	$(OCAMLC) -o ifdef.exe $(COMMON) ifdef.ml
	$(OCAMLC) -o test_ifdef.exe -ppx ./ifdef.exe -dsource test_ifdef.ml
	./test_ifdef.exe

ifdef2:
	ocamlbuild -r -cflag -annot -package compiler-libs.common -package ppx_tools ifdef.native

## A proposal for replacing js_of_ocaml Camlp4 syntax extension with
## a -ppx filter

.PHONY: js_syntax
js_syntax:
	$(OCAMLC) -o js_syntax.exe $(COMMON) js_syntax.ml
	$(OCAMLC) -o test_ifdef.exe -i -ppx ./js_syntax.exe test_js.ml

js2:
	ocamlbuild -r -cflag -annot -package compiler-libs.common -package ppx_tools js_syntax.native


## A "toy" ocamldoc clone based on .cmti files

.PHONY: minidoc
minidoc:
	$(OCAMLC) -custom -o minidoc.exe $(COMMON) minidoc.ml
	$(OCAMLC) -c -bin-annot testdoc.mli
	./minidoc.exe testdoc.cmti

## Using the OCaml toplevel to evaluate expression during compilation

.PHONY: eval
eval:
	$(OCAMLC) -linkall -o eval.exe  $(COMMON) $(BYTECMP) $(TOPLVL) eval.ml
	$(OCAMLC) -o test_eval.exe -ppx ./eval.exe test_eval.ml
	./test_eval.exe

eval2:
	ocamlbuild -r -cflag -annot -package compiler-libs.common -package ppx_tools eval.native

## Example of code generation based on type declarations

.PHONY: ppx_builder
ppx_builder:
	$(OCAMLC) -linkall -o ppx_builder.exe  $(COMMON) ppx_builder.ml
	$(OCAMLC) -o test_builder.exe -ppx ./ppx_builder.exe -dsource test_builder.ml

## Import type definitions from other source files (e.g. to avoid code
## duplication between the .ml and .mli files)

.PHONY: copy_typedef
copy_typedef:
	$(OCAMLC) -linkall -o copy_typedef.exe  $(COMMON) copy_typedef.ml
	$(OCAMLC) -c -ppx ./copy_typedef.exe test_copy_typedef.mli
	$(OCAMLC) -o test_copy_typedef.exe -ppx ./copy_typedef.exe -dsource test_copy_typedef.ml


## Create mli files from ml files

.PHONY: nomli
nomli:
	$(OCAMLC) -linkall -o nomli.exe $(COMMON) $(BYTECMP) ../../tools/untypeast.cmo ../../tools/tast_iter.cmo nomli.ml
	./nomli.exe test_nomli.ml


## A port of pa_matches

.PHONY: matches
matches:
	$(OCAMLC) ppx_tools.cma -linkall -o ppx_matches.exe $(COMMON) ppx_matches.ml
	$(OCAMLC) -c -dsource -ppx ./ppx_matches.exe test_matches.ml

builder2:
	ocamlbuild -r -package compiler-libs.common -package ppx_tools ppx_builder.native

minidoc2:
	ocamlbuild -r -package compiler-libs.common -package ppx_tools minidoc.native

matches2:
	ocamlbuild -r -package compiler-libs.common -package ppx_tools ppx_matches.native

matches-t:
	ocamlfind ppx_tools/rewriter ./ppx_matches.native test_matches.ml

builder-t:
	ocamlfind ppx_tools/rewriter ./ppx_builder.native test_builder.ml

# nomli2:
# 	ocamlbuild -r -package compiler-libs.common -package ppx_tools nomli.native

nomli2:
	ocamlbuild -r -package compiler-libs.common -package ppx_tools -cflags -I,$(INCL) nomli.native

copy_td2:
	ocamlbuild -r -package compiler-libs.common -package ppx_tools -cflags -I,$(INCL) copy_typedef.native

unused2:
	ocamlbuild -r -package compiler-libs.common -package ppx_tools  -cflags -I,$(INCL) unused_exported_values.native

