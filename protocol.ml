open Core.Std


module File_spec = struct
	type t = {path: string; offset: int; length: int option} with sexp
end

module Data = struct
type t = S of string| File of File_spec.t with sexp
end

module Worker_announce = struct
	type t = Live of  int | Died of  int with sexp
end

module Worker_type = struct 
	type t = Mapper | Reducer  with sexp
end

module Request = struct
	type t = Init of string| Finish | Map of string * Data.t | Reduce of string * (Data.t list)| Ping
	with sexp 
end 

module Response = struct
	type t = | Ready 
      | Term of int | Error of string | Pong of int| Map of string * (string * Data.t) list 
	| Reduce of string * Data.t with sexp
	
end
