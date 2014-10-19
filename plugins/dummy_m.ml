let sleep = Unix.sleep
open Protocol
open Core.Std
open Async.Std


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
			printf "\nMapping key %s" key;
			sleep 1;
			let tag = if data mod 2 =1 then "odd" else "even" in
			[("sum", (cnv data))]
		
	end