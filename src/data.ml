open Printf
module ISet = Set.Make(Int)


let load_features file =
    let lines = Utils.read_lines file in
    let split = Str.split_delim (Str.regexp " ") in
    let rec loop split_lines = function
        | [] -> List.rev split_lines
        | h :: t ->
            let features_list = List.map int_of_string (split h) in
            ISet.of_list features_list :: (loop split_lines t) in
    loop [] lines

let load_labels file =
    List.map int_of_string (Utils.read_lines file)

let print_label l = l |> printf "%n\n"

let load ?labels features =
    let features = load_features features in
    let labels = match labels with
    | None -> List.map (fun _ -> None) features
    | Some labels -> List.map (fun l -> Some l) (load_labels labels) in
    List.combine features labels

let label (features, label) =
    match label with
    | None -> failwith "unlabeled example"
    | Some l -> l

let labels examples =
    List.map (fun (f, l) -> l) examples

let features (features, label) =
    features

let length = List.length

let all_features examples =
    let all = List.fold_left
        (fun s e -> ISet.union s (features e)) ISet.empty examples in
    ISet.elements all

let n_features examples =
    List.length (all_features examples)

(*
let print_example {_; universe} i =
    ISet.iter (fun f -> printf "%n %!" f) features.(n);
    match labels with
        | None -> ()
        | Some labels -> printf "# %n\n%!" labels.(n)
*)

(*
let random_feature {indices; features; _} =
    let random_example = features.(Utils.choose_random indices) in
    Utils.choose_random (ISet.elements random_example)
*)

let random_feature examples =
    let random_example_1 = features (Utils.choose_random examples) in
    let random_example_2 = features (Utils.choose_random examples) in
    let ex_1_minus_ex_2 = ISet.diff random_example_1 random_example_2 in
    if ISet.is_empty ex_1_minus_ex_2 then
        Utils.choose_random (ISet.elements random_example_1)
    else
        Utils.choose_random (ISet.elements ex_1_minus_ex_2)

let random_features examples n =
    let rec loop acc = function
        | 0 -> acc
        | n -> loop ((random_feature examples) :: acc) (n - 1) in
    loop [] n

let random_rule examples =
    ISet.mem (random_feature examples)

let split_impur impur rule examples =
    let append (left, right) e =
        if rule (features e) then
            (label e :: left, right) else (left, label e :: right) in
    let left, right = List.fold_left append ([], []) examples in
    ((impur left) +. (impur right)) /. 2.

exception Empty_list

(* m -- numbers of features to choose from *)
let gini_rule ?m:(m=0) examples =
    let n = length examples in
    let m = match m with
    | 0 -> n |> float_of_int |> sqrt |> int_of_float
    | m -> m in
    let random_feas = random_features examples m in
    let rec loop features impurs =
        match features with
        | [] -> List.rev impurs
        | h :: t ->
            let rule = fun e -> ISet.mem h e in
            let impur = split_impur Impurity.gini_impur rule examples in
            loop t (impur :: impurs) in
    let impurs = loop random_feas [] in
    let feas_impurs = List.combine random_feas impurs in
    let im (_, i) = i in
    let feas_impurs_sorted =
        List.sort (fun a b -> compare (im a) (im b)) feas_impurs in
    let best_fea =
        match feas_impurs_sorted with
        | [] -> raise Empty_list
        | (f, _) :: _ -> f in
    fun example -> ISet.mem best_fea example

let empty = []

let is_empty examples =
    examples = []

let first_label = function
    | (f, l) :: _ -> l
    | [] -> failwith "empty examples" ;;

let indices examples =
    List.init (List.length examples) (fun i -> i)

let random_label examples =
    let (f, l) = Utils.choose_random examples in l

let random_subset examples =
    Utils.sample_with_replace examples

let uniform_labels examples =
    let labels = labels examples in
    let rec uniform labels =
        match labels with
        | [] | [_] -> true
        | h1 :: h2 :: t ->
            if h1 = h2 then uniform (h2 :: t) else false in
    uniform labels

let split rule examples =
    let rec loop examples_l examples_r = function
        | [] -> (examples_l, examples_r)
        | e :: t ->
            match rule (features e) with
            | true -> loop (e :: examples_l) examples_r t
            | false -> loop examples_l (e :: examples_r) t in
    loop [] [] examples

let length examples =
    List.length examples

let random_example examples =
    Utils.choose_random examples

let append examples1 examples2 =
    List.append examples1 examples2

let get examples i =
    List.nth examples i

let fold_left f s examples =
    List.fold_left f s examples
