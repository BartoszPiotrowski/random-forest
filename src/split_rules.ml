let random_thr x =
    let min = Utils.min_list x in
    let x_no_min = List.filter (fun i -> i > min) x in
    (* check if uniform x *)
    if (List.length x_no_min) = 0 then let h :: _ = x in h
    else List.nth x_no_min (Random.int (List.length x_no_min))

let random_rule features _ =
    let n = Random.int (Data.ncol features) in
    let col = Data.col_to_list n features in
    (n, random_thr col)

let best_thr_impur impur x labels =
    let x_l = List.combine x labels in
    let x_l = List.sort (fun a b -> compare (fst a) (fst b)) x_l in
    let rec best_thr_impur_aux remaining_x_l best_thr best_impur =
        match remaining_x_l with
        | [] | [_]  -> best_thr, best_impur
        | (x1, l1) :: (x2, l2) :: t ->
            let new_thr = (x1 +. x2) /. 2. in
            let new_impur = Impurity.split_impur impur x_l new_thr in
            if new_impur < best_impur then
                best_thr_impur_aux ((x2, l2) :: t) new_thr new_impur
            else
                best_thr_impur_aux ((x2, l2) :: t) best_thr best_impur
    in best_thr_impur_aux x_l 0. 1.

let best_thr i x l = fst (best_thr_impur i x l)

let gini_one_rule features labels =
    let n = Random.int (Data.ncol features) in
    let col = Data.col_to_list n features in
    let labels = Data.labels_to_list labels in
    (n, best_thr Impurity.gini_impur col labels)

let gini_rule features labels =
    let ncols = Data.ncol features in
    let random_cols_rep = Utils.sample_with_return ncols ncols in
    let random_cols = IntSet.elements (IntSet.of_list random_cols_rep) in
    let labels = Data.labels_to_list labels in
    let rec aux rc thrs_impurs =
        match rc with
        | [] -> List.rev thrs_impurs
        | h :: t ->
                let col = Data.col_to_list h features in
                let thr_impur = best_thr_impur Impurity.gini_impur col labels in
                aux t (thr_impur :: thrs_impurs) in
    let thrs_impurs = aux random_cols [] in
    let cols_impurs = List.combine random_cols thrs_impurs in
    let im (_, (_, i)) = i in
    let (best_col, (best_thr, _)) :: _ =
        List.sort (fun a b -> compare (im a) (im b)) cols_impurs in
    best_col, best_thr
