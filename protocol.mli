(** Data pieces are kept in file system
*)
module File_spec :
  sig
    type t = { path : string; offset : int; length : int option; } with sexp
  end 
	
(** Data pieces module
*)	
module Data :
  sig
		(** Data are either string or stored in file *) 
    type t = S of string | File of File_spec.t with sexp
  end 
	
(** Protocol for workers to announce their presence *)	
module Worker_announce :
  sig
		 (** When worker start sends Live to controller,  if is going to die send Died
		message contans host name , port *)
    type t = Live of  int | Died of  int with sexp
    
  end 
	
(** Requests from controller *)	
module Request :
  sig
    type t =
        Init of  string (** prepares worker - load code from path specified *)
      | Finish  (** terminates worker *)
      | Map of string * Data.t
      | Reduce of string * Data.t list
      | Ping 
			with sexp 
  end 
	
module Response :
  sig
    type t =
			| Ready 
      | Term of int
      | Error of string
      | Pong of int
      | Map of string * (string * Data.t) list
      | Reduce of string * Data.t list
      with sexp
  end 
