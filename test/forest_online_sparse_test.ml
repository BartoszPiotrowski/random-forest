(* let () = Printf.printf "Start...\n"; flush stdout;; *)
open Forest_online.Make(Data)
open Data

let () = Printf.printf "Start...\n%!"
let examples =
    load "test/data/field_theory.features"
    ~labels:"test/data/field_theory.labels"
let labels_test = labels examples

let t0 = Sys.time()
let forest1 = forest examples

let t0 = Sys.time()
let preds1 = classify forest1 examples ;;

print_float (Utils.accuracy labels_test preds1);
