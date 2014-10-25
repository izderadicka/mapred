open Protocol
open Core.Std

module type Controlling =
	sig
		
val init: Sexp.t option-> unit
(** gets one key to be mapped, None if there are no more input data *)
val next_piece: unit -> (string * Data.t) option

(** combines all results from mapping phase, collects together by same key *)
val combine : (string ,(string * Data.t) list) Hashtbl.t -> (string * (Data.t list)) list

(** process results from reduce task *)

val process_result : (string * Data.t) -> unit

end

module type Mapping =
	sig
		type in_type
		type out_type
		val read_data: Data.t -> in_type
		val write_data: out_type -> Data.t
		val map: (out_type -> Data.t) -> string -> in_type -> (string * Data.t) list
		
	end
	
module type Reducing =
	sig
		type in_type
		type out_type
		val read_data: Data.t -> in_type
		val write_data: out_type -> Data.t
		val reduce: (Data.t -> in_type)  -> string -> Data.t list -> out_type
	end
	