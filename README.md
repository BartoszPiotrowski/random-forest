# Random forest

Random forest implementation, for tabular and sparse data. 

# Usage
```
make native

# Sparse version
./rfs.native \
    -train_x test/data/field_theory_train.features \
    -train_y test/data/field_theory_train.labels \
    -test_x  test/data/field_theory_test.features

# Tabular version
./rft.native \
    -train_x test/data/iris_train.features \
    -train_y test/data/iris_train.labels \
    -test_x  test/data/iris_test.features
```
