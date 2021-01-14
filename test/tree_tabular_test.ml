module Data = Data.Make(Tabular)
open Tree.Make(Data)
open Data

let random_tree = tree random_rule
let gini_tree = tree gini_rule

let examples_train =
    load "test/data/iris_train.features" ~labels:"test/data/iris_train.labels"
let examples_test =
    load "test/data/iris_test.features" ~labels:"test/data/iris_test.labels"
let labels_test = examples_test.labels

let tree1 = random_tree examples_train
let preds1 = classify examples_test tree1
let tree2 = gini_tree examples_train
let preds2 = classify examples_test tree2 ;;

print_float (Utils.accuracy (labels examples_test) preds1);
print_newline ();
print_float (Utils.accuracy (labels examples_test) preds2);
print_newline ();

