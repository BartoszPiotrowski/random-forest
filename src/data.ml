
module type DATA_CONCRETE = sig
    type example_features
    type label
    type indices = int list
    type features = example_features array
    type labels = label array
    type examples = {
        indices : indices;
        features : features;
        labels : labels option}
    type rule = example_features -> bool
    type split_rule = examples -> examples * examples
    val labels : examples -> label list
    val load_labels : string -> labels
    val load_features : string -> features
    val print_example : examples -> int -> unit
(*     val print_example_2 : example -> unit *)
    val print_label : label -> unit
    val random_rule : examples -> rule
    val gini_rule : ?m:int -> examples -> rule
end;;

module Make = functor (D : DATA_CONCRETE) -> struct
    include D

    let load ?labels features =
        let features = D.load_features features
        and labels = match labels with
        | None -> None
        | Some labels -> Some (D.load_labels labels) in
        let n = Array.length features in
        let indices = List.init n (fun x -> x) in
        {D.indices; D.features; D.labels}

    let indices {D.indices; D.features; _} =
        indices

    let labels {D.indices; D.features; D.labels} =
        match labels with
        | None -> failwith "no labels"
        | Some (l) -> Array.to_list l

    let print examples =
        let inds = indices examples in
        List.iter (D.print_example examples) inds

    let empty =
        {D.indices=[]; D.features=[||]; D.labels=Some [||]}

    let is_empty {D.indices; D.features; D.labels} =
        indices = []

    let first_label {D.indices; D.features; D.labels} =
        match indices, labels with
        | _, None -> failwith "unlabeled examples"
        | [], _ -> failwith "empty examples"
        | h :: t, Some labels -> labels.(h)

    let random_label {D.indices; D.features; D.labels} =
        match labels with
        | None -> failwith "unlabeled examples"
        | Some labels -> let i = Utils.choose_random indices in labels.(i)

    let random_subset {D.indices; D.features; D.labels} =
        let random_indices =
            Utils.sample_with_replace indices (List.length indices) in
        {D.indices=random_indices; D.features; D.labels}

    let uniform_labels {D.indices; D.features; D.labels} =
        match labels with
        | None -> failwith "unlabeled examples"
        | Some labels ->
            let rec uniform inds =
                match inds with
                | [] | [_] -> true
                | h1 :: h2 :: t ->
                    if labels.(h1) = labels.(h2) then uniform (h2 :: t) else false in
            uniform indices

    let split rule {D.indices; D.features; D.labels} =
        let rec loop inds_l inds_r = function
            | [] -> (inds_l, inds_r)
            | h :: t ->
                match rule features.(h) with
                | true -> loop (h :: inds_l) inds_r t
                | false -> loop inds_l (h :: inds_r) t in
        let inds_l, inds_r = loop [] [] indices in
        ({D.indices = inds_l; D.features; D.labels},
         {D.indices = inds_r; D.features; D.labels})

    let length {D.indices; D.features; D.labels} =
        List.length indices

    let random_example {D.indices; D.features; D.labels} =
        let i = Utils.choose_random indices in
        {D.indices=[i]; D.features=features; D.labels=labels}

    let add examples example =
        let max_i = Utils.max_list examples.indices in
        let new_indices = (max_i + 1) :: examples.indices in
        let features, label = example in
        let new_features =
            Array.append [|features|] examples.features in
        let new_labels =
            match examples.labels with
            | None -> failwith "cannot add labeled example to unlabeled examples"
            | Some ls -> Some (Array.append ls [|label|])
               (* previously (Array.append [|label|] ls) TODO investigate*)
        in
        {
            D.indices = new_indices;
            D.features = new_features;
            D.labels = new_labels
        }

    let append {D.indices=indices1; D.features=features1; D.labels=labels1}
               {D.indices=indices2; D.features=features2; D.labels=labels2} =
    assert (features1 == features2);
    assert (labels1 == labels2);
    let new_indices = List.append indices1 indices2 in
    {D.indices=new_indices; D.features=features1; D.labels=labels1}

    let get {D.indices; D.features; D.labels} i =
        {D.indices=[i]; D.features; D.labels}

    let fold_left f s examples =
        let f' acc i = f acc (get examples i) in
        List.fold_left f' s examples.indices
end
