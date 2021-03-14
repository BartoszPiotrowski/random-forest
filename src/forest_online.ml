module Make = functor (Data : Tree_online.DATA) -> struct
    module Tree = Tree_online.Make(Data)

    let empty = []

    let add forest example =
        let updated_trees =
(*
            let parmap_forest = Parmap.L forest in
            Parmap.parmap (fun tree -> Tree.add tree example) parmap_forest in
*)
            List.map (fun tree -> Tree.add tree example) forest in
        let n_trees = List.length forest in
        let add_new_tree = (n_trees = 0) || (Random.int n_trees = 0) in
        if add_new_tree then Tree.leaf example :: updated_trees
        else updated_trees

    let forest examples =
        Data.fold_left add empty examples

(*
    let forest tree n examples =
        let initseg = List.init n (fun i -> i) in
        List.map (fun _ -> tree (Data.random_subset examples)) initseg
*)

(*
    let vote votes =
        let tbl = Hashtbl.create 10 in (* about 10 classes assumed *)
        let update cl =
            if Hashtbl.mem tbl cl then
                Hashtbl.replace tbl cl ((Hashtbl.find tbl cl) + 1)
            else
                Hashtbl.add tbl cl 1
        in
        List.iter update votes;
        let update_max_cl cl v (max_cls, max_v) =
            match compare v max_v with
            | 1  -> ([cl], v)
            | 0  -> (cl :: max_cls, v)
            | -1 -> (max_cls, max_v)
            | _  -> failwith "Illegal value of compare." in
        let max_freq_cls = Hashtbl.fold update_max_cl tbl ([], 1) in
        match max_freq_cls with
        | ([], _) ->
            failwith "At least one class needs to have maximal frequency."
        | ([x], _) -> x
        | (l, _) -> Utils.choose_random l
*)

    let vote votes =
        let sorted = List.sort compare votes in
        let rec loop occ sorted =
            match sorted, occ with
            | [], _ -> occ
            | h :: t, [] -> loop [(h, 1)] t
            | h :: t, (e, c) :: t2 ->
                if h = e
                then loop ((e, c + 1) :: t2) t
                else loop ((h, 1) :: (e, c) :: t2) t
        in
        let occurs = loop [] sorted in
        let sum = float_of_int (List.length votes) in
        let freqs =
            List.map (fun (e, c) -> (e, (float_of_int c) /. sum)) occurs in
        List.sort (fun (_, c1) (_, c2) -> compare c2 c1) freqs

    let classify forest example =
        let votes = List.map (Tree.classify example) forest in
        match vote votes with
        | (e, _) :: _ -> e
        | [] -> failwith "empty list of voting scores"

    let score forest example =
        let votes = List.map (Tree.classify example) forest in
        vote votes
end
