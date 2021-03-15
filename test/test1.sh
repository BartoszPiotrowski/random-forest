
echo Training...
./rfs.native \
    -train_x test/data/features-tactic.train.2.small.features \
    -train_y test/data/features-tactic.train.2.small.labels \
    -test_x  test/data/features-tactic.valid.2.small.features \
    -n $1 \
    > test/data/features-tactic.valid.2.small.preds
echo Training done.
echo Accuracy:
./utils/accuracy.py \
    test/data/features-tactic.valid.2.small.labels \
    test/data/features-tactic.valid.2.small.preds
