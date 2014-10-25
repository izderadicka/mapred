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
	   vals := (range count)
	|None -> ()
	
let next_piece () = 
match !vals with 
| hd::tl -> vals := tl; 
printf "\nReading piece %d" hd;
Some (Int.to_string hd, Data.S (Int.to_string hd))
| [] -> None




let combine res = 
 printf "\nCombining results";
 let tbl = String.Table.create () in
 let rec add l =
	match l with 
	| (key,v)::t ->  ( match Hashtbl.find tbl key with
		| Some ev -> Hashtbl.replace tbl ~key ~data: (v :: ev); add t
		| None -> ignore (Hashtbl.add tbl ~key ~data:[v]) ; add t
		)
	|[] -> ()
 
	in Hashtbl.iter res ~f: (fun ~key ~data -> ignore key; add data);
	Hashtbl.to_alist tbl	

let process_result (key, res)  = 
let res =  match res with
	| Data.S s -> s
	| (Data.File {path;_}) -> sprintf "File:%s" path
	in printf "\nResult for %s is %s" key res

end

(* register itself with controller *)
let () =
	Controller_plug.set_ctl (module M : Ifc.Controlling);
	printf "\nInitialized plugin dummy"