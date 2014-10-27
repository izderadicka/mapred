

let () =
	let open Re_pcre in
	let l =  split ~rex:(regexp "[ \t\n]+") "aa bb cc" in
	List.iter (fun s -> print_endline s) l