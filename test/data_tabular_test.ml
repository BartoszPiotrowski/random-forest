open Data.Make(Tabular);;

let examples = load "test/data/iris.features" ~labels:"test/data/iris.labels" in
print examples
