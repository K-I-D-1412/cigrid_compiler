.PHONY: all clean

all: cigrid

cigrid:
	ocamlbuild -use-ocamlfind -pkg str main.native
	mv main.native cigrid

clean:
	ocamlbuild -clean
	$(RM) -f cigrid