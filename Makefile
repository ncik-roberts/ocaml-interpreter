all:
	ocamlbuild -use-ocamlfind top.native
clean:
	ocamlbuild -clean
	rm a.out
