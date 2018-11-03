ocamlopt -c hw2.ml
ocamlopt -c hw2_tests.ml
ocamlopt -o hw2 hw2.cmx hw2_tests.cmx
./hw2