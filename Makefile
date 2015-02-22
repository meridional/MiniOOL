all:
	ocamllex minioolLEX.mll
	ocamlyacc minioolYACC.mly
	ocamlc -c minioolYACC.mli
	ocamlc -c minioolLEX.ml
	ocamlc -c minioolYACC.ml
	ocamlc -c miniool.ml
	ocamlc -o miniool minioolLEX.cmo minioolYACC.cmo miniool.cmo
