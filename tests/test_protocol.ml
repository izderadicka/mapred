open OUnit2
open Protocol
open Core.Std

let test_serialization _ctx =
	let d = Data.S "aaa" in
	let se= Data.sexp_of_t d in
	let s = Sexp.to_string se in
	printf "\n%s \n" s;
	let nse = Sexp.of_string s in
	let nd= Data.t_of_sexp nse in
	match nd with
	| S text -> assert_equal text "aaa"
	| _ -> assert_failure "incorrect type"
	
let test_serialization2 _ctx =
	let open File_spec in
	let req = Request.Map ("key1", (Data.File {path="path";offset=0;length=None})) in
	let resp = Response.Map ("key1", [("imk1", Data.S "hello"); ("imk2", Data.S "word")]) in
	printf "\n%s\n" (Sexp.to_string (Request.sexp_of_t req)) ;
	printf "\n%s\n" (Sexp.to_string (Response.sexp_of_t resp))
	
	
let tests = "All" >::: [
	"test_serialication" >:: test_serialization;
	"test_seriallization2" >:: test_serialization2
	]
	
let () =
	run_test_tt_main tests
	
	