module Data = Data.Make(Sparse)
open Forest.Make(Data)
open Data

(* parsing arguments *)

(* default values *)
let train_x = ref ""
let train_y = ref ""
let test_x = ref ""
let n = ref 10
let m = ref 0
let max_depth = ref 10
let split_rule = ref "gini"
let data_type = ref "sparse"

let set_rule = function
    | "gini" -> split_rule := "gini"
    | "random" -> split_rule := "random"
    | _ -> raise (Arg.Bad("Bad split rule; available: gini, random."))

let speclist =
    [
        ("-train_x", Arg.Set_string train_x, "Training data, features.");
        ("-train_y", Arg.Set_string train_y, "Training data, labels.");
        ("-test_x", Arg.Set_string test_x, "Testing data, features.");
        ("-n", Arg.Set_int n, "Number of trees.");
        ("-m", Arg.Set_int m, "Number of features to choose from.");
        ("-max_depth", Arg.Set_int max_depth, "Maximal depth of trees.");
        ("-split_rule", Arg.Symbol (["gini"; "random"], set_rule),
            "Split rule for building trees.");
    ]
let usage = "Train a random forest model and predict for test examples."
let () = Arg.parse
    speclist (fun x -> raise (Arg.Bad ("Bad argument : " ^ x))) usage

(* end of parsing arguments *)


let train_examples = load !train_x ~labels:!train_y
let test_examples = load !test_x
let split_rule = match !split_rule with
    | "random" -> random_rule
    | "gini" -> gini_rule ~m:!m
    | _ -> failwith "Bad random rule specification."
let tree = Tree.tree split_rule ~max_depth:!max_depth
let forest = forest tree !n train_examples
let preds = classify forest test_examples
let () = List.iter print_label preds
