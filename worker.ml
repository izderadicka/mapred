open Core.Std
open Async.Std
open Protocol

let write_response w resp =
	Writer.write_sexp w (Response.sexp_of_t resp);
	Writer.flushed w

let start_server port =
	Tcp.Server.create
	 ~on_handler_error: `Raise
	(Tcp.on_port port)
	(fun _addr r w ->
		Reader.read_sexp r 
		>>= (fun res ->
			match res with
			| `Ok se ->
				begin 
				match Request.t_of_sexp se with
				| Ping -> 
					printf "\nController Ping on %d" port;
					write_response w (Response.Pong port)
				| Init (Worker_type.Mapper, path) ->
					printf "\nInitializing Mapper %d from %s"  port path;
					write_response w (Response.Ready Worker_type.Mapper)
				| _ -> failwith "Not Implemented"
				end
			|`Eof -> return ()
		))
	
		
let run  port ctl_host ctl_port =
	start_server port
	>>= (fun _s ->
	Tcp.with_connection (Tcp.to_host_and_port ctl_host ctl_port)
	 (fun _s _r w ->
		 Writer.write_sexp w (Worker_announce.sexp_of_t (Worker_announce.Live port));
		 Writer.flushed w
		) 
		)
	>>| (fun () -> printf "\nWorker started on port %d" port) 
	>>= never
		
let cmd = Command.async_basic
	~summary: "Map Reduce worker"
	Command.Spec. (
		empty
		+> flag "-port" (required int) ~doc: "Port to listen on for contoller commands"
		+> flag "-ctl-host" (optional_with_default "localhost" string) ~doc: "Controller host"
		+> flag "-ctl-port" (optional_with_default 8765 int) ~doc: "Controller port"
		
		)
		(fun port ctl_host ctl_port () -> run  port ctl_host ctl_port)
		
let () =
	Command.run ~version:"0.1" cmd
		