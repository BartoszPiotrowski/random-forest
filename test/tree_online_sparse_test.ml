open Tree_online.Make(Data)
open Printf
open Utils

let () = printf "Loading training data... %!"
let features = load_features "test/data/field_theory_train.features"
let labels = load_labels "test/data/field_theory_train.labels"
let () = printf "Done.\n%!"
let () = printf "Number of unique labels: %n\n" (List.length (Utils.uniq labels))
let () = printf "Number of examples: %n\n" (List.length features)
let examples = List.combine features labels
let examples = List.map Data.labeled examples
let tree1 = tree examples
let preds1 = List.map (fun e -> classify e tree1) examples
let () = print_float (accuracy labels preds1)

