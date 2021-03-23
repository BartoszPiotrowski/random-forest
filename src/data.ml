open Printf
module ISet = Set.Make(Int)

type features = ISet.t
type 'a example = (features * ('a option))
type 'a examples = 'a example list
type direction = Left | Right
type rule = features -> direction

let label (features, label) =
    match label with
    | None -> failwith "unlabeled example"
    | Some l -> l

let labels examples =
    List.map label examples

let unlabeled features =
    (ISet.of_list features, None)

let labeled (features, label) =
    (ISet.of_list features, Some label)

let features (features, label) =
    features

let length = List.length

let all_features examples =
    let all = List.fold_left
        (fun s e -> ISet.union s (features e)) ISet.empty examples in
    ISet.elements all

let n_features examples =
    List.length (all_features examples)

let random_feature examples =
    let ex1 = Utils.choose_random examples in
    let complem e = label e <> label ex1 && features e <> features ex1 in
    let examples_ex1 = List.filter complem examples in
    let ex2 = try Utils.choose_random examples_ex1 with _ -> ex1 in
    let feas =
        if ex1 = ex2 then features ex1 else
        let ex1', ex2' = if Random.int 2 = 0 then ex1, ex2 else ex2, ex1 in
        let diff = ISet.diff (features ex1') (features ex2') in
        if ISet.is_empty diff then ISet.diff (features ex2') (features ex1')
        else diff in
    Utils.choose_random (ISet.elements feas)

let is_splitting examples f =
    let is_mem e = ISet.mem f (features e) in
    let in_some = List.fold_left (fun b e -> b || is_mem e) false examples in
    let in_all = List.fold_left (fun b e -> b && is_mem e) true examples in
    in_some && (not in_all)

(* returns deduplicated list of splitting features *)
let random_features examples n =
    let feas = List.init n (fun _ -> random_feature examples) in
    let feas = ISet.elements (ISet.of_list feas) in
    List.filter (is_splitting examples) feas

let is_splitting examples f =
    let is_mem e = ISet.mem f (features e) in
    let in_some = List.fold_left (fun b e -> b || is_mem e) false examples in
    let in_all = List.fold_left (fun b e -> b && is_mem e) true examples in
    in_some && (not in_all)


let is_empty examples =
    examples = []

let indices examples =
    List.init (List.length examples) (fun i -> i)

let random_label examples =
    let (f, l) = Utils.choose_random examples in
    match l with
    | None -> failwith "unlabeled example"
    | Some l -> l

let random_subset examples =
    Utils.sample_with_replace examples (length examples)

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
            | Left -> loop (e :: examples_l) examples_r t
            | Right -> loop examples_l (e :: examples_r) t in
    loop [] [] examples

let length examples =
    List.length examples

let random_example examples =
    Utils.choose_random examples

let add examples example =
    example :: examples

let fold_left f s examples =
    List.fold_left f s examples

let random_rule examples =
    fun example ->
        match ISet.mem (random_feature examples) example with
        | true -> Left
        | false -> Right

let split_impur impur rule examples =
    let append (left, right) e =
        if rule (features e) then
            (label e :: left, right) else (left, label e :: right) in
    let left, right = List.fold_left append ([], []) examples in
    let el = float_of_int (List.length left) in
    let er = float_of_int (List.length right) in
    let e = float_of_int (List.length examples) in
    let fl = sqrt (el /. e) in
    let fr = sqrt (er /. e) in
    ((impur left) *. fl +. (impur right) *. fr)
(*     ((impur left) +. (impur right)) /. 2. *)

exception Rule_not_found
(* m -- numbers of random features to choose from *)
let gini_rule ?(m=1) examples =
    let t = Sys.time () in
    let random_feas = random_features examples m in
    let () = Printf.printf "%f\n%!" (Sys.time () -. t) in
    if random_feas = [] then raise Rule_not_found else
    let impur_from_fea f =
        split_impur Impurity.gini_impur (ISet.mem f) examples in
    let impurs = List.map impur_from_fea random_feas in
    let impurs_feas = List.combine impurs random_feas in
    let best_impur, best_fea = Utils.min_list impurs_feas in
    fun example ->
        match ISet.mem best_fea example with
        | true -> Left
        | false -> Right

