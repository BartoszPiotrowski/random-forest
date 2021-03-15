open Forest_online.Make(Data)
open Utils
open Printf

(* parsing arguments *)

(* default values *)
let train_x = ref ""
let train_y = ref ""
let test_x = ref ""
let pred_y = ref ""
let n = ref 10
let m = ref 0

let speclist =
    [
        ("-train_x", Arg.Set_string train_x, "Training data, features.");
        ("-train_y", Arg.Set_string train_y, "Training data, labels.");
        ("-test_x", Arg.Set_string test_x, "Testing data, features.");
        ("-pred_y", Arg.Set_string pred_y, "Predicted labels for testing data.");
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
    (fun forest example -> add forest (Data.labeled example))
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
