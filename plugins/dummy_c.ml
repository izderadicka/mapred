open Protocol
open Core.Std
open Interfaces

let  range n = 
let rec range n l =
	if  n = 0 then l
	else range (n-1) (n::l) in
range n [] 

let vals = ref (range 10)




module M : Ifc.Controlling =
	struct
let next_piece () = 
match !vals with 
| hd::tl -> vals := tl; Some (Int.to_string hd, Data.S (Int.to_string hd))
| [] -> None




let combine res = 
 let tbl = String.Table.create () in
 let rec add l =
	match l with 
	| (key,v)::t ->  ( match Hashtbl.find tbl key with
		| Some ev -> Hashtbl.replace tbl ~key ~data: (v @ ev); add t
		| None -> ignore (Hashtbl.add tbl ~key ~data:v) ; add t
		)
	|[] -> ()
 
	in add res;
	Hashtbl.to_alist tbl	

let process_results (key, res)  = 
printf "\nResult for %s" key;
let rec print = function
	| [] -> ()
	| e :: t -> 
	 let sep = match t with 
			| [] -> ", "
			| _ -> ""
	 in
	 match e with
	| Data.S s -> printf "%s%s" s sep; print t
	| (Data.File {path;_}) -> printf "File:%s%s" path sep;  print t
in
print res


end