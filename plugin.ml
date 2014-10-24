open Core.Std
open Async.Std

let load fname =
	let fname = Dynlink.adapt_filename fname in
	Sys.file_exists fname
	>>| function
	| `Yes ->
			begin
				try
					Dynlink.loadfile fname
				with
				
				| (Dynlink.Error err) -> printf "\nERROR loading plugin: %s" (Dynlink.error_message err) ; ignore (exit 1)
				| _ -> failwith "Unknow error while loading plugin"
			end
	| _ ->
			failwith "Plugin file does not exist"