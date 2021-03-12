open Tree_online.Make(Data)
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
let tree1 = tree examples_train ;;
let preds1 = classify examples_test tree1;;
print_float (Utils.accuracy labels_test preds1);;
