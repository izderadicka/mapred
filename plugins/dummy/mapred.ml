let sleep = Unix.sleep
open Protocol
open Core.Std
open Async.Std
open Worker_plug

let random_error key p1 p2 = 
if (Random.float 1.0) <= p1 then
			begin
				
				if (Random.float 1.0) <= p2 then
					(printf "\nRandomly ending worker for key %s" key;
						Core.Std.printf "\nRandomly ending worker for key %s" key;
						Caml.exit 1)
				else
					( printf "\nRandomly failing mapping for key %s" key; failwith "Worker failure")
			end


module M: Ifc.Mapping =
struct
	type in_type = int
	type out_type = int
	
	let read_data = function
		| Data.S s -> Int.of_string s
		| _ -> failwith "Invalid data transport representation"
	
	let write_data v =
		Data.S (Int.to_string v)
	
	let map cnv key data =
		sleep 1;
		random_error key 0.1 0.1;                     
		printf "\nMapping key %s" key;
		let tag = if data mod 2 =1 then "odd" else "even" in
		[(tag, (cnv data))]
	
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
	
	let reduce reader key data =
		random_error key 0.3 0.1;
		let sum = List.fold ~init:0
				~f: (fun s x -> s + x)
				(List.map data ~f: ( fun x -> reader x))
		in sum
	
end

let () =
	Random.self_init ();
	printf "\nPluging dummy init";
	set_mapper (module M: Ifc.Mapping);
	set_reducer (module R: Ifc.Reducing)