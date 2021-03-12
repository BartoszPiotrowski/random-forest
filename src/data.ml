
module type DATA_CONCRETE = sig
    type label = int
    type features
    type indices = int list
    type example = (features * (label option))
    type examples = {
        indices : int list;
        universe : (int, example) Hashtbl.t}
    type rule = features -> bool
    type split_rule = examples -> examples * examples
    val labels : examples -> label list
    val features : example -> features
    val label : example -> label
    val load_labels : string -> label list
    val load_features : string -> features list
(*     val print_example : examples -> int -> unit *)
    val print_label : label -> unit
    val random_rule : examples -> rule
    val gini_rule : ?m:int -> examples -> rule
end;;

module Make = functor (D : DATA_CONCRETE) -> struct
    include D

    let load ?labels features =
        let features = D.load_features features in
        let labels = match labels with
        | None -> List.map (fun _ -> None) features
        | Some labels -> List.map (fun l -> Some l) (D.load_labels labels) in
        let features_labels = List.combine features labels in
        let universe = Hashtbl.create 10000 in
        let indices = List.map
            (fun (f, l) ->
                let e = (f, l) in
                let h = Hashtbl.hash e in
                Hashtbl.add universe h e; h)
            features_labels in
        {indices; universe}

    let indices {D.indices; D.universe} =
        indices

(*
    let print examples =
        let inds = indices examples in
        List.iter (D.print_example examples) inds
*)

    let empty =
        let universe = Hashtbl.create 10000 in
        {D.indices=[]; D.universe=universe}

    let is_empty {indices; universe} =
        indices = []

    let first_label {D.indices; D.universe} =
        let i = match indices with
        | i :: _ -> i
        | [] -> failwith "empty examples" in
        label (Hashtbl.find universe i)

    let random_label {D.indices; D.universe} =
        let i = Utils.choose_random indices in
        label (Hashtbl.find universe i)

    let random_subset {D.indices; D.universe} =
        let random_indices =
            Utils.sample_with_replace indices (List.length indices) in
        {D.indices=random_indices; D.universe}

    let uniform_labels examples =
        let labels = labels examples in
        let rec uniform labels =
            match labels with
            | [] | [_] -> true
            | h1 :: h2 :: t ->
                if h1 = h2 then uniform (h2 :: t) else false in
        uniform labels

    let split rule {D.indices; D.universe} =
        let rec loop inds_l inds_r = function
            | [] -> (inds_l, inds_r)
            | h :: t ->
                match rule (features (Hashtbl.find universe h)) with
                | true -> loop (h :: inds_l) inds_r t
                | false -> loop inds_l (h :: inds_r) t in
        let inds_l, inds_r = loop [] [] indices in
        ({D.indices = inds_l; D.universe},
         {D.indices = inds_r; D.universe})

    let length {D.indices; D.universe} =
        List.length indices

    let random_example {D.indices; D.universe} =
        let i = Utils.choose_random indices in
        {D.indices=[i]; D.universe=universe}

    let add {D.indices; D.universe} (features, label) =
        let example = (features, Some label) in
        let i = Hashtbl.hash example in
        Hashtbl.add universe i example;
        {D.indices = [i]; D.universe = universe}

    let append {D.indices=indices1; D.universe=universe1}
               {D.indices=indices2; D.universe=universe2} =
    assert (universe1 == universe2);
    let new_indices = List.append indices1 indices2 in
    {D.indices=new_indices; D.universe=universe1}

    let get {D.indices; D.universe} i =
        {D.indices=[i]; D.universe}

    let fold_left f s examples =
        let f' acc i = f acc (get examples i) in
        List.fold_left f' s examples.indices
end
