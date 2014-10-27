let hard_exit=exit
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
				
				| (Dynlink.Error err) -> let msg = sprintf "\nERROR loading plugin: %s" (Dynlink.error_message err) in 
				failwith msg
				| _ -> failwith "Unknow error while loading plugin"
			end
	| _ ->
			failwith "Plugin file does not exist"