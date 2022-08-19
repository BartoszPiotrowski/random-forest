OCB_FLAGS = -I src -pkgs str,ounit2,parmap
OCB = ocamlbuild $(OCB_FLAGS)

all: classification

clean:
	$(OCB) -clean

classification: sanity
	$(OCB) rfso.native

# check that packages can be found
sanity:
	ocamlfind query str ounit2 parmap

test: classification
	./rfso.native \
		-train_x test/data/field_theory_train.features \
		-train_y test/data/field_theory_train.labels \
		-test_x  test/data/field_theory_test.features \
		-pred_y  test/data/field_theory_test.preds
