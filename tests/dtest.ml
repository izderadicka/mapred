open Dtest_ep

let load_plug fname = 
  let fname = Dynlink.adapt_filename fname in
  if Sys.file_exists fname then
		try
			Dynlink.loadfile fname
		with 
		| (Dynlink.Error err) as e -> 	 print_endline ("ERROR loading plugin: " ^ (Dynlink.error_message err) ); raise e
		| _ -> failwith "Unknow error while loading plugin"
	else 
	  failwith "Plugin file does not exist"
		

let () =
	load_plug "/home/ivan/workspace/mapred/tests/_build/dtest_plug.cmo";
	let module M = (val get_plugin () : PLUG) in
	print_endline (M.hello ())
	
	
	