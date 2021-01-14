if [ $# -eq 0 ]
then test_files=`find test -maxdepth 1 -name '*.ml' `; verbose=0
else test_files=$@; verbose=1
fi

for t in $test_files
do
    l=${t%.*}.log
    b=${t%.*}.build
    e=${t%.*}.byte
    rm -r $b
    ocamlbuild -clean &> /dev/null
    echo "Running test $t"
    if [ $verbose -eq 1 ]
    then
        ocamlbuild -pkgs str,csv,ounit2,parmap -build-dir $b -I src $e
        if [ 0 -eq $? ]
        then
            ./$b/test/`basename $e`
        fi

    else
        ocamlbuild -pkgs str,csv,ounit2,parmap \
            -build-dir $b -I src ${t%.*}.byte -- > $l
        if [ 0 -eq $? ]
        then
            echo "OK. Log in $l"
        else
            echo "FAILED. Log in $l"
        fi
    fi
    echo ""
done
ocamlbuild -clean &> /dev/null
rm oUnit-all_tests* 2> /dev/null
exit 0
