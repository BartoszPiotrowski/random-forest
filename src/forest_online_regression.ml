module Make = functor (Data : Tree_online.DATA) -> struct
    module Tree = Tree_online_regression.Make(Data)

    let empty = []

    let add ?(min_impur=0.1) ?(n_trees=100) ?(remove_old=false)
        forest example =
        let n = List.length forest in
        let add_tree = (n = 0) || ((Random.int n = 0) && n < n_trees) in
        let del_tree = remove_old && n >= n_trees in
        let forest = if del_tree then Utils.remove_last forest else forest in
        let updated_trees =
            Utils.map (fun tree -> Tree.add ~min_impur tree example) forest in
        if add_tree then Tree.leaf example :: updated_trees else updated_trees

    let forest examples =
        Data.fold_left add empty examples

    let vote votes =
        let freqs = Utils.freqs votes in
        List.sort (fun (_, c1) (_, c2) -> compare c2 c1) freqs

    let predict forest example =
        let votes = Utils.map (Tree.classify example) forest in
        match votes with
        | [] -> failwith "empty list of voting scores"
        | votes -> Utils.avg votes

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
