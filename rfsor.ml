open Forest_online_regression.Make(Data)
open Utils
open Printf

(* parsing arguments *)

(* default values *)
let train_x = ref ""
let train_y = ref ""
let test_x = ref ""
let pred_y = ref ""
let n_trees = ref 320
let min_impur = ref 0.1
let remove_old = ref false
let pred_type = ref "label"
let threshold = ref infinity

let speclist =
    [
        ("-train_x", Arg.Set_string train_x, "Training data, features.");
        ("-train_y", Arg.Set_string train_y, "Training data, labels.");
        ("-test_x", Arg.Set_string test_x, "Testing data, features.");
        ("-pred_y", Arg.Set_string pred_y, "Predictions for testing data.");
        ("-n_trees", Arg.Set_int n_trees, "Max number of trees.");
        ("-min_impur", Arg.Set_float min_impur, "Min impurity to trigger split.");
        ("-remove_old", Arg.Set remove_old, "Remove old trees.");
        ("-threshold", Arg.Set_float threshold, "Cast preds to bin classes, using threshold.");
    ]
let usage = "Train an online random forest model and predict for test examples."
let () = Arg.parse
    speclist (fun x -> raise (Arg.Bad ("Bad argument : " ^ x))) usage

(* end of parsing arguments *)

let () = printf "Loading training data... %!"
let train_features = load_features !train_x
let train_labels = load_labels !train_y
let () = printf "Done.\n%!"
let () = printf "Loading testing data... %!"
let test_features = load_features !test_x
let () = printf "Done.\n%!"
let learn forest features_labels = List.fold_left
    (fun forest example -> add
        ~min_impur:!min_impur
        ~n_trees:!n_trees
        ~remove_old:!remove_old
        forest (Data.labeled example))
    forest features_labels
let train_features_labels = List.combine train_features train_labels
let () = printf "Training random forest... %!"
let forest = learn [] train_features_labels
let () = printf "Done.\n%!"
let () = printf "Making predictions... %!"
let preds = Utils.map (predict forest)
    (Utils.map Data.unlabeled test_features)
let () = printf "Done.\n%!"
let preds_file = open_out !pred_y
let () = List.iter (fun p ->
  if !threshold < infinity then
    let p = if p < !threshold then 0 else 1 in
    fprintf preds_file "%n\n" p
  else fprintf preds_file "%f\n" p) preds
let () = close_out preds_file
let () = printf "\n## Stats of the trained forest ##\n%!"
let () = stats forest
