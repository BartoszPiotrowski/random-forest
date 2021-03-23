open Forest_online.Make(Data)
open Utils
open Printf

(* parsing arguments *)

(* default values *)
let train_x = ref ""
let train_y = ref ""
let test_x = ref ""
let pred_y = ref ""
let n_trees = ref 100
let max_depth = ref 100
let min_impur = ref 0.5
let n_feas = ref 1
let output_type = ref "labels"

let speclist =
    [
        ("-train_x", Arg.Set_string train_x, "Training data, features.");
        ("-train_y", Arg.Set_string train_y, "Training data, labels.");
        ("-test_x", Arg.Set_string test_x, "Testing data, features.");
        ("-pred_y", Arg.Set_string pred_y, "Predictions for testing data.");
        ("-n_trees", Arg.Set_int n_trees, "Max number of trees.");
        ("-max_depth", Arg.Set_int max_depth, "Max depth of trees.");
        ("-min_impur", Arg.Set_float min_impur, "Min impurity to trigger split.");
        ("-n_feas", Arg.Set_int n_feas, "Number of random features in splits.");
        ("-output_type", Arg.Set_string output_type, "Either labels or ranks.");
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
        ~n_feas:!n_feas
        ~max_depth:!max_depth
        ~min_impur:!min_impur
        forest (Data.labeled example))
    forest features_labels
let train_features_labels = List.combine train_features train_labels
let () = printf "Training random forest... %!"
let forest = learn [] train_features_labels
let () = printf "Done.\n%!"
let () = printf "Making predictions... %!"
let preds = List.map (classify forest) (List.map Data.unlabeled test_features)
let () = printf "Done.\n%!"
let preds_file = open_out !pred_y
let () = List.iter (fun p -> fprintf preds_file "%n\n" p) preds
let () = close_out preds_file
let () = printf "Stats of the trained forest:\n%!"
let () = stats forest
