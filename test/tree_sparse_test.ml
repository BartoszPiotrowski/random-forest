module Data = Data.Make(Sparse)
open Tree.Make(Data)
open Data
open Printf

let examples_train =
    load "test/data/field_theory_train.features"
    ~labels:"test/data/field_theory_train.labels"
let examples_test =
    load "test/data/field_theory_test.features"
    ~labels:"test/data/field_theory_test.labels"
let labels_test = labels examples_test ;;

printf "tree1\n";;
let tree1 = tree random_rule examples_train ;;
let preds1 = classify examples_test tree1;;
print_float (Utils.accuracy labels_test preds1);;
print_newline ();;
print_newline ();;

printf "tree2\n";;
let tree2 = tree gini_rule examples_train
let preds2 = classify examples_test tree2 ;;
print_float (Utils.accuracy (labels examples_test) preds2);
print_newline ();
