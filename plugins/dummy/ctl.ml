open Protocol
open Core.Std
open Async.Std


let  range n = 
let rec range n l =
	if  n = 0 then l
	else range (n-1) (n::l) in
range n [] 

let vals = ref (range 10)




module M : Ifc.Controlling =
	struct
		
let init s =
	match s with 
	|Some s -> let count = Int.t_of_sexp s in
	   vals := (range count);
		return ()
	|None -> return ()
	
let next_piece () = 
match !vals with 
| hd::tl -> vals := tl; 
printf "\nReading piece %d" hd;
return (Some (Int.to_string hd, Data.S (Int.to_string hd)))
| [] -> return None

let process_result (tbl)  = 
Hashtbl.iter tbl ~f: (fun ~key ~data ->
let res =  match data with
	| Data.S s -> s
	| (Data.File {path;_}) -> sprintf "File:%s" path
	in printf "\nResult for %s is %s" key res)

end

(* register itself with controller *)
let () =
	Controller_plug.set_ctl (module M : Ifc.Controlling);
	printf "\nInitialized plugin dummy"