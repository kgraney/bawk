
OBJS = utile.cmo scanner.cmo parser.cmo bawk.cmo

bawk: $(OBJS)
	ocamlc -o bawk $(OBJS)

scanner.ml: scanner.mll
	ocamllex scanner.mll

parser.ml parser.mli: parser.mly
	ocamlyacc parser.mly

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

clean:
	rm -f bawk *.cmi *.cmo scanner.ml parser.ml parser.mli

.PHONY: clean

bawk.cmo : scanner.cmo parser.cmi ast.cmi
bawk.cmx : scanner.cmx parser.cmx ast.cmi
parser.cmo : ast.cmi parser.cmi
parser.cmx : ast.cmi parser.cmi
scanner.cmo : utile.cmi parser.cmi
scanner.cmx : utile.cmx parser.cmx
utile.cmo : utile.cmi
utile.cmx : utile.cmi
parser.cmi : ast.cmi
