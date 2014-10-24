let ctl = ref None

let get_ctl () : (module Ifc.Controlling) =
	match !ctl with
	| Some m -> m
	| None -> failwith "No controller module loaded"
	
let set_ctl c=
	ctl:= Some c
	
