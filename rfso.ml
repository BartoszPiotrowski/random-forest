open Forest_online.Make(Data)
open Data

(* parsing arguments *)

(* default values *)
let train_x = ref ""
let train_y = ref ""
let test_x = ref ""
let n = ref 10
let m = ref 0

let speclist =
    [
        ("-train_x", Arg.Set_string train_x, "Training data, features.");
        ("-train_y", Arg.Set_string train_y, "Training data, labels.");
        ("-test_x", Arg.Set_string test_x, "Testing data, features.");
    ]
let usage = "Train an online random forest model and predict for test examples."
let () = Arg.parse
    speclist (fun x -> raise (Arg.Bad ("Bad argument : " ^ x))) usage

(* end of parsing arguments *)

let train_examples = load !train_x ~labels:!train_y
let test_examples = load !test_x
let forest = forest train_examples
let preds = classify forest test_examples
let () = List.iter print_label preds
