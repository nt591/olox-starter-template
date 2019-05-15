open OUnit2

let tests = "test Identity" >::: [
  "it returns what you input" >:: (fun _ -> assert_equal (Identity.id 1) 1);
]

let _ = run_test_tt_main tests