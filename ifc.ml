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