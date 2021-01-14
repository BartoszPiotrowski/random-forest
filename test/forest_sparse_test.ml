
(* let () = Printf.printf "Start...\n"; flush stdout;; *)
module Data = Data.Make(Data_sparse)
open Forest.Make(Data)
open Data

let () = Printf.printf "Start...\n"
let examples =
    load "test/data/field_theory.features" ~labels:"test/data/field_theory.labels"
let labels_test = labels examples

let tree1 = Tree.tree random_rule
let tree2 = Tree.tree gini_rule

let n = 10

let () = Printf.printf "Training forest 1...\n%!"
let t0 = Sys.time()
let forest1 = forest tree1 n examples
let () = Printf.printf "Done. (%.2f s)\n%!" (Sys.time() -. t0)

let () = Printf.printf "Training forest 2...\n%!"
let t0 = Sys.time()
let forest2 = forest tree2 n examples
let () = Printf.printf "Done. (%.2f s)\n%!" (Sys.time() -. t0)

let () = Printf.printf "Classifying with forest 1...\n%!"
let t0 = Sys.time()
let preds1 = classify forest1 examples
let () = Printf.printf "Done. (%.2f s)\n%!" (Sys.time() -. t0)

let () = Printf.printf "Classifying with forest 2...\n%!"
let t0 = Sys.time()
let preds2 = classify forest2 examples
let () = Printf.printf "Done. (%.2f s)\n%!" (Sys.time() -. t0)

;;
print_float (Utils.accuracy labels_test preds1);
print_newline ();
print_float (Utils.accuracy labels_test preds2);
print_newline ();
