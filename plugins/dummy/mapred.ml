let sleep = Unix.sleep
open Protocol
open Core.Std
open Async.Std
open Worker_plug

module M: Ifc.Mapping =
	struct
		type in_type = int
		type out_type = int
		
		let read_data = function
			| Data.S  s -> Int.of_string s
			| _ -> failwith "Invalid data transport representation"
			
		let write_data v = 
			Data.S (Int.to_string v)
			
		let map cnv key data =
			
			sleep 1;
			(* if key = "5" then ( printf "\Systematically failing mapping for key %s" key; failwith "Mapping failure") *)
			if (Random.float 1.0) <= 0.2 then
			begin
				
				if (Random.float 1.0) <= 0.2 then
					(printf "\nRandomly ending worker for key %s" key;
					Core.Std.printf "\nRandomly ending worker for key %s" key;
					Caml.exit 1)
				else
				( printf "\nRandomly failing mapping for key %s" key; failwith "Mapping failure")
			end
			else
			printf "\nMapping key %s" key;
			let tag = if data mod 2 =1 then "odd" else "even" in
			[(tag, (cnv data))]
		
	end
	
	
let () = 
   Random.self_init ();
	 set_mapper (module M: Ifc.Mapping)