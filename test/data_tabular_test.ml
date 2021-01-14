open Data.Make(Data_tabular);;

let examples = load "test/data/iris.features" ~labels:"test/data/iris.labels" in
print examples
