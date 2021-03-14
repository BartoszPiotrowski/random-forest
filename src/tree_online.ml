module type DATA = sig
    type label
    type features
    type example = features * (label option)
    type examples = example list
    type direction = Left | Right
    type rule = features -> direction
    val is_empty : examples -> bool
    val add : examples -> example -> examples
    val random_label : examples -> label
    val features : example -> features
    val split : rule -> examples -> examples * examples
    val gini_rule : ?m:int -> examples -> rule
    val random_example : examples -> example
    val fold_left : ('a -> example -> 'a) -> 'a -> examples -> 'a
    val label : example -> label
    val labels : examples -> label list
end

module Make = functor (Data : DATA) -> struct

    type tree =
        | Node of Data.rule * tree * tree
        | Leaf of Data.label * Data.examples

    let leaf example =
        Leaf (Data.label example, [example])

    (* returns Node(split_rule, Leaf (label1, stats1), Leaf(label2, stats2)) *)
    let make_new_node examples =
        let rule = Data.gini_rule examples in
        let examples_l, examples_r = Data.split rule examples in
        if Data.is_empty examples_l || Data.is_empty examples_r
        then Leaf(Data.random_label examples, examples)
        else Node(rule,
            Leaf(Data.random_label examples_l, examples_l),
            Leaf(Data.random_label examples_r, examples_r))

    let extend examples =
        let labels = Data.labels examples in
        let imp = Impurity.gini_impur labels in
        imp > 0.5
    (* TODO more sophisticated condition needed *)

    (* pass the example to a leaf; if a condition is satisfied, extend the tree *)
    let add tree example =
        let rec loop = function
            | Node (rule, tree_l, tree_r) ->
                (match rule (Data.features example) with
                | Left  -> Node(rule, loop tree_l, tree_r)
                | Right -> Node(rule, tree_l, loop tree_r))
            | Leaf (label, examples) ->
                let examples = Data.add examples example in
                if extend examples then make_new_node examples
                else Leaf (label, examples)
        in
        loop tree

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

end
