let mapper = ref None

let get_mapper (): (module Ifc.Mapping) =
	match !mapper with
	| Some m -> m
	| None -> failwith "Mapper is not initialized"
	
let set_mapper  m =
	mapper:= Some m