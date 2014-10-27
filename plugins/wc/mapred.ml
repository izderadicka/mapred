open Protocol
open Core.Std
open Worker_plug

module M: Ifc.Mapping =
struct
	type in_type = string list
	type out_type = int
	
	let read_data = function
		| Data.S s -> let open Re_pcre in split ~rex: (regexp "[ \t\n.,]+") s
		| _ -> failwith "Invalid data transport representation"
	
	let write_data v =
		Data.S (Int.to_string v)
	
	let map write_data _key data =
		(* simpler alternative : *)
		(*List.map data ~f: (fun k -> (k, (Data.S  "1")))      *)
		
		(* more complicated and only marginally faster *)
		let tbl = String.Table.create () in
		List.iter data ~f: (fun key ->
						match Hashtbl.find tbl key with
						| Some i -> Hashtbl.replace tbl ~key ~data: (i +1)
						| None -> Hashtbl.add_exn tbl ~key ~data:1);
		List.map (Hashtbl.to_alist tbl) ~f: ( fun (k, v) -> (k, (write_data v)))
	
end

module R: Ifc.Reducing =
struct
	type in_type = int
	type out_type = int
	
	let read_data = function
		| Data.S s -> Int.of_string s
		| _ -> failwith "Invalid data transport representation"
	
	let write_data v =
		Data.S (Int.to_string v)
	let reduce read_data _key data =
		List.fold data ~init:0 ~f: (fun acc x -> acc + (read_data x))
end

let () =
	set_mapper (module M: Ifc.Mapping);
	set_reducer (module R: Ifc.Reducing)