let mapper = ref None
let reducer = ref None

let get_mapper (): (module Ifc.Mapping) =
	match !mapper with
	| Some m -> m
	| None -> failwith "Mapper is not initialized"
	
let set_mapper  m =
	mapper:= Some m
	
	
let get_reducer (): (module Ifc.Reducing) =
	match !reducer with
	| Some r -> r
	| None -> failwith "Reducer is not initialized"
	
let set_reducer r =
	reducer:= Some r