open Protocol

module type Controlling =
	sig
(** gets one key to be mapped, None if there are no more input data *)
val next_piece: unit -> (string * Data.t) option

(** combines all results from mapping phase, collects together by same key *)
val combine : (string * (Data.t list)) list -> (string * (Data.t list)) list

(** process results from reduce task *)

val process_results : (string * (Data.t list)) -> unit

end

module type Mapping =
	sig
		type in_type
		type out_type
		val read_data: Data.t -> in_type
		val write_data: out_type -> Data.t
		val map: (out_type -> Data.t) -> string -> in_type -> (string * Data.t) list
		
	end