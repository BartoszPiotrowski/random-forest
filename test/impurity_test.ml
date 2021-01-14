open Impurity
open OUnit2


let test_array_1 = [0;1;0;1];;
let gini_1 = gini_impur test_array_1

let test_gini_1 = fun _ -> assert_equal 0.5 gini_1 ~printer:string_of_float
(* https://towardsdatascience.com/gini-index-vs-information-entropy-7a7e4fed3fcb *)


let test_array_2 = [0;0;0;1];;
let gini_2 = gini_impur test_array_2

let test_gini_2 = fun _ -> assert_equal 0.375 gini_2 ~printer:string_of_float
(* https://towardsdatascience.com/gini-index-vs-information-entropy-7a7e4fed3fcb *)

let x_l_1 = [(1., 0); (2., 1); (3., 0); (4., 0)]
let split_impur_1 = split_impur gini_impur x_l_1 2.5;;

let test_split_impur_1 = fun _ -> assert_equal 0.25 split_impur_1 ~printer:string_of_float

let test_array_3 = List.init 1000 (fun _ -> Random.int 10);;
let gini_3 = gini_impur test_array_3

let test_gini_3 = fun _ -> assert_bool "non_zero_gini" (gini_3 > 0.)


let test_values_1 = [1.;2.;3.;4.];;
let test_labels_1 = [0; 0; 0; 1 ];;
let thr_1, impur_1 = best_split gini_impur test_values_1 test_labels_1;;

let test_thr_1 = fun _ -> assert_equal 3.5 thr_1 ~printer:string_of_float
let test_impur_1 = fun _ -> assert_equal 0. impur_1 ~printer:string_of_float


let test_values_2 = [1.;2.;3.;0.];;
let test_labels_2 = [0; 0; 0; 1 ];;
let thr_2, impur_2 = best_split gini_impur test_values_2 test_labels_2;;

let test_thr_2 = fun _ -> assert_equal 0.5 thr_2  ~printer:string_of_float
let test_impur_2 = fun _ -> assert_equal 0. impur_2 ~printer:string_of_float


let test_values_3 = [1.;2.;3.;4.];;
let test_labels_3 = [0; 1; 0; 0 ];;
let thr_3, impur_3 = best_split gini_impur test_values_3 test_labels_3;;

let test_thr_3 = fun _ -> assert_bool "too_small_thr" (thr_3 > 1.)
let test_impur_3 = fun _ -> assert_bool "zero_impur" (impur_3 > 0.)

let test =
    "all_tests" >::: [
        "thr_1" >:: test_thr_1;
        "impur_1" >:: test_impur_1;
        "thr_2" >:: test_thr_2;
        "impur_2" >:: test_impur_2;
        "thr_3" >:: test_thr_3;
        "impur_3" >:: test_impur_3;
        "gini_1" >:: test_gini_1;
        "gini_2" >:: test_gini_2;
        "gini_3" >:: test_gini_3;
        "split_impur_1" >:: test_split_impur_1;
    ]

let _ = run_test_tt_main test;;
