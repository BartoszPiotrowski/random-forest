module Tree = Tree_online.Make(Data)
module Forest = Forest_online.Make(Data)
open Data
open Forest
open Printf
open Utils

let () = printf "Loading training data... %!"
let features = load_features "test/data/field_theory.features"
let labels = load_labels "test/data/field_theory.labels"
let () = printf "Done.\n%!"

let empty = []

let learn forest features_labels = List.fold_left
(*     (fun forest out -> add forest ((fst out), Some (snd out))) *)
    (fun forest out -> add forest (Data.labeled out))
    forest features_labels

let features_labels = List.combine features labels

let () = printf "Training random forest in online fashion... %!"
let forest1 = learn empty features_labels
let () = printf "Done.\n%!"

let () = printf "Making predictions... %!"
let preds1 = List.map (Forest.classify forest1)
    (List.map Data.unlabeled features) ;;
let () = printf "Done.\n%!"
let () = printf "Accuracy on train: %f%!" (Utils.accuracy labels preds1)