module type PLUG =
	sig
		val hello: unit -> string
	end
	
let p = ref None
let get_plugin () : (module  PLUG)  =
	match !p with 
	| Some s -> s
	| None -> failwith "No plugin loaded"
	
	
