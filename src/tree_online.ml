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
    val gini_rule : examples -> split_rule
    val length : examples -> int
    val label : example -> label
    val examples_of_1 : example -> examples
    val add : examples -> example -> examples
end

module Make = functor (Data : DATA) -> struct

    type tree =
        | Node of Data.split_rule * tree * tree
        | Leaf of Data.label * Data.examples

    let leaf example = Leaf (Data.label example, Data.examples_of_1 example)

    (* returns Node(split_rule, Leaf (label1, stats1), Leaf(label2, stats2)) *)
    let make_new_node examples =
        let split_rule = Data.gini_rule examples in
        let examples_l, examples_r = split_rule examples in
        Node(split_rule,
            Leaf(Data.random_label examples_l, examples_l),
            Leaf(Data.random_label examples_r, examples_r))

    let extend examples =
        if Data.length examples > 10 then true else false
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


