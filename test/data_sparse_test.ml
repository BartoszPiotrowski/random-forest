open Data.Make(Data_sparse)

let examples =
    load "test/data/field_theory.features" ~labels:"test/data/field_theory.labels"
let () = print examples

