all: run

run: m.cmo error.cmo lexer.cmo parser.cmo pp.cmo simple_checker.cmo main.cmo
	ocamlc -g -o run m.cmo error.cmo lexer.cmo parser.cmo pp.cmo simple_checker.cmo main.cmo

error.cmo : error.ml m.cmo 
	ocamlc -g -c error.ml

m.cmo : m.ml
	ocamlc -g -c m.ml

pp.cmo : pp.ml m.cmo
	ocamlc -g -c pp.ml

parser.ml: parser.mly m.cmo
	ocamlyacc parser.mly

parser.mli: parser.mly m.cmo
	ocamlyacc parser.mly

parser.cmi: parser.mli
	ocamlc -g -c parser.mli

parser.cmo: parser.ml parser.cmi
	ocamlc -g -c parser.ml

simple_checker.cmo : simple_checker.ml simple_checker.mli
	ocamlc -g -c simple_checker.mli
	ocamlc -g -c simple_checker.ml

main.cmo : main.ml m.cmo pp.cmo
	ocamlc -g -c main.ml

lexer.cmo: lexer.ml error.cmo
	ocamlc -g -c lexer.ml

lexer.ml: lexer.mll parser.cmo m.cmo
	ocamllex lexer.mll

clean:
	rm -f *.cmx *.cmi parser.mli parser.ml lexer.ml run *.o *.cmo
