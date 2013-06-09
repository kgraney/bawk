
OBJS = utile.cmo scanner.cmo parser.cmo ast.cmo bawk.cmo

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


ast.cmo: ast_types.cmi ast.cmi
ast.cmx: ast_types.cmi ast.cmi
bawk.cmo: scanner.cmo parser.cmi ast.cmi
bawk.cmx: scanner.cmx parser.cmx ast.cmx
parser.cmo: ast.cmi parser.cmi
parser.cmx: ast.cmx parser.cmi
scanner.cmo: utile.cmi parser.cmi
scanner.cmx: utile.cmx parser.cmx
utile.cmo: utile.cmi
utile.cmx: utile.cmi
ast.cmi: ast_types.cmi
ast_types.cmi:
parser.cmi: ast.cmi
utile.cmi:
