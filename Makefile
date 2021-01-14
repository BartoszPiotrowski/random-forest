OCB_FLAGS = -I src -pkgs str,csv,ounit2,parmap
OCB = ocamlbuild $(OCB_FLAGS)

all: native byte

clean:
	$(OCB) -clean

native: sanity
	$(OCB) rfs.native
	$(OCB) rft.native

byte: sanity
	$(OCB) rfs.byte
	$(OCB) rft.byte

# check that packages can be found
sanity:
	ocamlfind query str csv ounit2 parmap

test: native
	./rfs.native \
		-train_x test/data/field_theory_train.features \
		-train_y test/data/field_theory_train.labels \
		-test_x  test/data/field_theory_test.features
	./rft.native \
		-train_x test/data/iris_train.features \
		-train_y test/data/iris_train.labels \
		-test_x  test/data/iris_test.features
