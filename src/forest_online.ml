module Make = functor (Data : Tree_online.DATA) -> struct
    module Tree = Tree_online.Make(Data)

    let empty = []

    let add ?(n_feas=1) ?(min_impur=0.5) ?(max_depth=100) ?(n_trees=100)
        forest example =
        let n = List.length forest in
        let add_tree = (n = 0) || ((Random.int n = 0) && n < n_trees) in
        let updated_trees =
            List.map
            (fun tree -> Tree.add ~n_feas ~min_impur ~max_depth tree example)
            forest in
        if add_tree then Tree.leaf example :: updated_trees else updated_trees

    let forest examples =
        Data.fold_left add empty examples

    let vote votes =
        let freqs = Utils.freqs votes in
        List.sort (fun (_, c1) (_, c2) -> compare c2 c1) freqs

    type 'a pred =
        | Label of 'a
        | Ranking of ('a list)
        | Ranking_with_scores of (('a * float) list)

    let score forest example =
        let votes = List.map (Tree.classify example) forest in
        vote votes

    let predict ?(pred_type="label") forest example =
        let scores = score forest example in
        if pred_type = "label" then
            let l = match scores with
            | (l, _) :: _ -> l
            | [] -> failwith "empty list of voting scores" in
            Label l
        else if pred_type = "rank" then
            Ranking (List.map (fun (l, _) -> l) scores)
        else failwith "unknown prediction type specified"

    let classify forest example =
        let scores = score forest example in
        match scores with
        | [] -> failwith "empty list of voting scores"
        | (l, _) :: _ -> l

    let stats forest =
        let l = List.length forest in
        let ds_sum = List.fold_left (fun s t -> s + Tree.depth t) 0 forest in
        let ds_avg = (float_of_int ds_sum) /. (float_of_int l) in
        let ns_sum = List.fold_left (fun s t -> s + Tree.max_node t) 0 forest in
        let nns_sum = List.fold_left (fun s t -> s + Tree.n_nodes t) 0 forest in
        let nns_avg = (float_of_int nns_sum) /. (float_of_int l) in
        let ns_sum_lab =
            List.fold_left (fun s t -> s + Tree.max_labels_node t) 0 forest in
        let ns_avg = (float_of_int ns_sum) /. (float_of_int l) in
        let ns_avg_lab = (float_of_int ns_sum_lab) /. (float_of_int l) in
        let () = Printf.printf "\nNumber of trees: %n\n" l in
        let () = Printf.printf "Avg depth of trees: %f\n" ds_avg in
        let () = Printf.printf "Avg n. of nodes in trees: %f\n" nns_avg in
        let () = Printf.printf "Avg largest node of trees: %f\n" ns_avg in
        let () = Printf.printf
            "Avg largest n. of labels in node of trees: %f\n" ns_avg_lab in
        let () = Printf.printf "\n" in ()

end
