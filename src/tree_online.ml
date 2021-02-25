open Printf

module type DATA = sig
    type indices = int list
    type example
    type examples
    type label
    type rule = example -> bool
    type split_rule = examples -> examples * examples
    val uniform_labels : examples -> bool
    val indices : examples -> indices
    val is_empty : examples -> bool
    val random_label : examples -> label
    val random_subset : examples -> examples
    val split : rule -> split_rule
    val split_rev : split_rule -> rule
    val gini_rule : ?m:int -> examples -> rule
    val length : examples -> int
    val label : example -> label option
    val examples_of_1 : example -> examples
    val add : examples -> example -> examples
    val random_example : examples -> example
    val fold_left : ('a -> example -> 'a) -> 'a -> examples -> 'a
    val print : examples -> unit
(*     val print_example_2 : example -> unit *)
    val labels : examples -> label list
end

module Make = functor (Data : DATA) -> struct

    type tree =
        | Node of Data.split_rule * tree * tree
        | Leaf of Data.label * Data.examples

    let leaf example =
        let l = match Data.label example with
        | None -> failwith "label required"
        | Some l -> l in
        Leaf (l, Data.examples_of_1 example)

    (* returns Node(split_rule, Leaf (label1, stats1), Leaf(label2, stats2)) *)
    let make_new_node examples =
        let split_rule = Data.split (Data.gini_rule examples) in
        let examples_l, examples_r = split_rule examples in
        if Data.is_empty examples_l || Data.is_empty examples_r
        then Leaf(Data.random_label examples, examples)
        else
            Node(split_rule,
                Leaf(Data.random_label examples_l, examples_l),
                Leaf(Data.random_label examples_r, examples_r))

(*
    let extend examples =
        if Data.length examples > 10 then true else false
*)

    let extend examples =
        let labels = Data.labels examples in
        let imp = Impurity.gini_impur labels in
        imp > 0.5

    (* TODO more sophisticated condition needed *)

    (* pass the example to a leaf; if a condition is satisfied, extend the tree *)
    let add tree example =
        let rec loop = function
            | Node (split_rule, left_tree, right_tree) ->
                let rule = Data.split_rev split_rule in
                (match rule example with
                | true -> Node(split_rule, loop left_tree, right_tree)
                | false -> Node(split_rule, left_tree, loop right_tree))
            | Leaf (label, examples) ->
                let examples = Data.add examples example in
                if extend examples then make_new_node examples
                else Leaf (label, examples)
        in
        loop tree

    let tree examples =
        let example = Data.random_example examples in
        Data.fold_left add (leaf example) examples

    let classify examples tree =
        let rec loop tree examples =
            match tree with
            | Leaf (cls, _) ->
                List.map (fun i -> (i, cls)) (Data.indices examples)
            | Node (split_rule, tree_l, tree_r) ->
                let examples_l, examples_r = split_rule examples in
                (loop tree_l examples_l) @
                (loop tree_r examples_r)
        in
        let inds_labels = loop tree examples in
        let inds = Data.indices examples in
        List.map (fun i -> List.assoc i inds_labels) inds
end


