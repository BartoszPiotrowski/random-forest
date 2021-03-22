module type DATA = sig
    type features
    type 'a example = features * ('a option)
    type 'a examples = 'a example list
    type direction = Left | Right
    type rule = features -> direction
    exception No_splitting_feature
    val is_empty : 'a examples -> bool
    val add : 'a examples -> 'a example -> 'a examples
    val features : 'a example -> features
    val split : rule -> 'a examples -> 'a examples * 'a examples
    val gini_rule : 'a examples -> rule
    val random_label : 'a examples -> 'a
    val random_example : 'a examples -> 'a example
    val fold_left : ('a -> 'b example -> 'a) -> 'a -> 'b examples -> 'a
    val label : 'a example -> 'a
    val labels : 'a examples -> 'a list
end

module Make = functor (Data : DATA) -> struct

    type 'a tree =
        | Node of Data.rule * ('a tree) * ('a tree)
        | Leaf of 'a * ('a Data.examples)

    let leaf example =
        Leaf (Data.label example, [example])

    (* returns Node(split_rule, Leaf (label1, stats1), Leaf(label2, stats2)) *)
    let make_new_node examples =
        let () = Printf.printf "make_new_node\n" in
        try
            let rule = Data.gini_rule examples in
            let examples_l, examples_r = Data.split rule examples in
            let () = List.iter
                (fun x -> Printf.printf "l %n\n" (Data.label x)) examples_l in
            let () = List.iter
                (fun x -> Printf.printf "r %n\n" (Data.label x)) examples_r in
            let () = Printf.printf "success\n" in
            Node(rule,
                Leaf(Data.random_label examples_l, examples_l),
                Leaf(Data.random_label examples_r, examples_r))
        with Data.No_splitting_feature ->
            let () = Printf.printf "fail\n" in
            Leaf(Data.random_label examples, examples)
(*             assert false = (Data.is_empty examples_l); *)
(*             assert false = (Data.is_empty examples_r); *)

    let init_cond examples depth =
        let labels = Data.labels examples in
        let imp = Impurity.gini_impur labels in
        imp > 0.3 && depth < 200 && List.length examples > 5

    (* pass the example to a leaf; if a condition is satisfied, extend the tree *)
    let add (tree : 'a tree) (example : 'a Data.example) : 'a tree =
        let rec loop depth = function
            | Node (rule, tree_l, tree_r) ->
                (match rule (Data.features example) with
                | Left  -> Node(rule, loop (depth + 1) tree_l, tree_r)
                | Right -> Node(rule, tree_l, loop (depth + 1) tree_r))
            | Leaf (label, examples) ->
(*            Printf.eprintf "depth: %n\n" depth; *)
(*            Printf.eprintf "#examples: %n\n%!" (List.length examples) ; *)
                let examples = Data.add examples example in
                if init_cond examples depth then make_new_node examples
                else Leaf (label, examples)
        in
        loop 0 tree

    let tree examples =
        let example = Data.random_example examples in
        Data.fold_left add (leaf example) examples

    let classify example tree =
        let rec loop tree =
            match tree with
            | Leaf (cls, _) -> cls
            | Node (rule, tree_l, tree_r) ->
                (match rule (Data.features example) with
                | Left  -> loop tree_l
                | Right -> loop tree_r)
        in loop tree

    let depth tree =
        let rec loop d t =
            match t with
            | Node(_, tl, tr) -> max (loop (d+1) tl) (loop (d+1) tr)
            | Leaf(_) -> d
        in loop 0 tree

    let max_node tree =
        let rec loop t =
            match t with
            | Node(_, tl, tr) -> max (loop tl) (loop tr)
            | Leaf(_, es) -> List.length es
        in loop tree

    let max_labels_node tree =
        let rec loop t =
            match t with
            | Node(_, tl, tr) -> max (loop tl) (loop tr)
            | Leaf(_, es) -> List.length (Utils.uniq (Data.labels es))
        in loop tree

end
