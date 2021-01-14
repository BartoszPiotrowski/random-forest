module Data = Data.Make(Tabular)
open Forest.Make(Data)
open Data

let examples_train =
    load "test/data/iris_train.features" ~labels:"test/data/iris_train.labels"
let examples_test =
    load "test/data/iris_test.features" ~labels:"test/data/iris_test.labels"
let labels_test = labels examples_test

let tree1 = Tree.tree random_rule
let tree2 = Tree.tree gini_rule

let n = 100

let forest1 = forest tree1 n examples_train
let forest2 = forest tree2 n examples_train

let preds1 = classify forest1 examples_test
let preds2 = classify forest2 examples_test;;

print_float (Utils.accuracy labels_test preds1);
print_newline ();
print_float (Utils.accuracy labels_test preds2);
print_newline ();
