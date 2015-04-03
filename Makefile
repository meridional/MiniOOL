all:
	ocamlc -c minioolEval.ml
	ocamllex minioolLEX.mll
	ocamlyacc minioolYACC.mly
	ocamlc -c minioolYACC.mli
	ocamlc -c minioolLEX.ml
	ocamlc -c minioolYACC.ml
	ocamlc -c miniool.ml
	ocamlc -o miniool minioolEval.cmo minioolLEX.cmo minioolYACC.cmo miniool.cmo 
