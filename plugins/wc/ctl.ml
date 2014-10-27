open Protocol
open Core.Std
open Async.Std

module M: Ifc.Controlling =
struct
	let reader = ref None
	
	let init param =
		match param with 
		| Some p ->
		let filename = String.t_of_sexp p in
		Reader.open_file filename
		>>| (fun r -> reader:= Some r)
		| None -> failwith "filename param is required"
	
	let next_piece () =
		match !reader with
		| Some r ->
				begin
					Reader.read_line r
					>>| function
					| `Ok s ->
							begin
								match String.split ~on:'@' s with
								| [key; _; data] -> Some (key, (Data.S data))
								| _ -> failwith "Invalid input"
							end
					| `Eof -> don't_wait_for (Reader.close r) ; None
				end
		| None -> failwith "File reader is not initialized"
	
	let process_result res =
		let to_int = function
			| Data.S s -> Int.of_string s
			| _ -> assert false
		in
		let sres = List.sort (Hashtbl.to_alist res)
		~cmp: (fun (_, v1) (_, v2) ->
			- (Int.compare (to_int v1) (to_int v2)))
		in
		let rec print l n =
			match l with
			| [] -> ()
			| (k,d)::tl -> if n>0 then printf "\nWord %s count %d " k (to_int d); print tl (n-1)
			
		in 
		print sres 10
	
end

(* register itself with controller *)
let () =
	Controller_plug.set_ctl (module M : Ifc.Controlling);
	printf "\nInitialized plugin wc"