#!/bin/bash

ocamlc -i compute.ml > compute.mli
ocamlc -c compute.mli
ocamlc -i readfilef.ml > readfilef.mli
ocamlc -c readfilef.mli
ocamlc -i gui.ml > gui.mli
ocamlc -c gui.mli
ocamlc -c compute.ml
ocamlc -c readfilef.ml
ocamlc -c gui.ml
ocamlc -c main.ml
ocamlc -o compute str.cma graphics.cma compute.cmo readfilef.cmo gui.cmo main.cmo
