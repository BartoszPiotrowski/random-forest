OCB_FLAGS = -I src -pkgs str,csv,ounit2,parmap
OCB = ocamlbuild $(OCB_FLAGS)

all: classification regression

clean:
	$(OCB) -clean

classification: sanity
	$(OCB) rfso.native

regression: sanity
	$(OCB) rfsor.native

# check that packages can be found
sanity:
	ocamlfind query str csv ounit2 parmap

test_c: classification
	./rfso.native \
		-train_x test/data/field_theory_train.features \
		-train_y test/data/field_theory_train.labels \
		-test_x  test/data/field_theory_test.features \
		-pred_y  test/data/field_theory_test.preds

test_r: regression
	./rfsor.native \
		-train_x test/data/synthetic_binary_train.features \
		-train_y test/data/synthetic_binary_train.labels \
		-test_x  test/data/synthetic_binary_test.features \
		-pred_y  test/data/synthetic_binary_test.preds \
		-threshold 0.5
