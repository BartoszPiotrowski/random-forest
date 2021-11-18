module Forest = Forest_online_regression.Make(Data)
open Data
open Forest
open Printf
open Utils

let () = printf "Loading training data... %!"
let features = load_features "test/data/synthetic_binary_train.features"
let labels = load_labels "test/data/synthetic_binary_train.labels"
let () = printf "Done.\n%!"
let () = printf "Number of unique labels: %n\n" (List.length (Utils.uniq labels))
let () = printf "Number of examples: %n\n" (List.length features)
let examples = List.combine features labels
let empty = []
let learn forest examples = List.fold_left
    (fun forest out -> add ~min_impur:0.1 forest (Data.labeled out))
    forest examples
let () = printf "Training random forest in online fashion... %!"
let forest1 = learn empty examples
let () = printf "Done.\n%!"
let () = printf "Making predictions... %!"
let preds1 = List.map (Forest.predict forest1)
    (List.map Data.unlabeled features) ;;
let () = printf "Done.\n%!"
let () = printf "RMSE on train: %f\n%!" (Utils.rmse labels preds1)
let () = Forest.stats forest1
