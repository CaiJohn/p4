OCAMLMAKEFILE = OCamlMakefile

SOURCES = bvset.mli bvset.ml disassembler.mli disassembler.ml dfa.ml main.ml
RESULT = dfa
OCAMLLDFLAGS = -g

include $(OCAMLMAKEFILE)
