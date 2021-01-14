
module type DATA_CONCRETE = sig
    type example
    type label
    type indices = int list
    type features = example array
    type labels = label array
    type examples = {
        indices : indices;
        features : features;
        labels : labels option}
    type rule = example -> bool
    type split_rule = examples -> examples * examples
    val labels : examples -> label list
    val load_labels : string -> labels
    val load_features : string -> features
    val print_example : examples -> int -> unit
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

    let print examples =
        let inds = indices examples in
        List.iter (D.print_example examples) inds

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

(* let split x  = Utils.time (split x) *)

end
