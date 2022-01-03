open Tree_online_regression.Make(Data)
open Printf
open Utils

let () = printf "Loading training data... %!"
let features = load_features "test/data/synthetic_binary_train.features"
let labels = load_labels "test/data/synthetic_binary_train.labels"
let () = printf "Done.\n%!"
let () = printf "Number of unique labels: %n\n" (List.length (Utils.uniq labels))
let () = printf "Number of examples: %n\n" (List.length features)
let examples = List.combine features labels
let examples = List.map Data.labeled examples
let tree = tree examples
let preds = List.map (fun e -> classify e tree) examples
(* let () = List.iter (fun x -> printf "%f\n" x) preds *)
let () = printf "RMSE: %f\n" (rmse labels preds)


