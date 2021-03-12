OCB_FLAGS = -I src -pkgs str,csv,ounit2,parmap
OCB = ocamlbuild $(OCB_FLAGS)

all: native byte

clean:
	$(OCB) -clean

native: sanity
	$(OCB) rfso.native

byte: sanity
	$(OCB) rfso.native

# check that packages can be found
sanity:
	ocamlfind query str csv ounit2 parmap

test: native
	./rfso.native \
		-train_x test/data/field_theory_train.features \
		-train_y test/data/field_theory_train.labels \
		-test_x  test/data/field_theory_test.features
