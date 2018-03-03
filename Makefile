all:
	ocamlbuild -use-ocamlfind top.byte
clean:
	ocamlbuild -clean
	rm a.out
