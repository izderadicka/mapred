open Core.Std

open Async.Std

open Protocol

type worker_status =
	| Ready | Initialized_mapper | Initialized_reducer | Working | Dead

type worker_remote =
	{ address : Host_and_port.t; mutable last_active : Time.t;
		mutable status : worker_status
	}

let ctl = ref None

let available_workers = Host_and_port.Table.create ()
let free_workers_reader, free_workers_writer = Pipe.create ()
let mapper_workers_reader, mapper_workers_writer = Pipe.create ()

let sent_map_pieces = String.Hash_set.create ()
let map_results = String.Table.create ()

let start port =
	Tcp.Server.create ~on_handler_error: `Raise (Tcp.on_port port)
		(fun addr r _w ->
					(Reader.read_sexp r) >>=
					(function
						| `Eof -> return ()
						| `Ok s ->
								let addr = Socket.Address.Inet.to_host_and_port addr in
								let create_addr p =
									Host_and_port.create ~host: (Host_and_port.host addr)
										~port: p
								in
								(match Worker_announce.t_of_sexp s with
									| Live p ->
											let worker_addr = create_addr p
											in
											(printf "\nWorker live %s"
													(Host_and_port.to_string worker_addr);
												ignore
													(Hashtbl.add available_workers ~key: worker_addr
															~data:
															{
																address = worker_addr;
																last_active = Time.now ();
																status = Ready;
															});
												ignore (Pipe.write free_workers_writer worker_addr);
												return ())
									| Died p ->
											let worker_addr = create_addr p
											in
											(printf "\nWorker down %s"
													(Host_and_port.to_string worker_addr);
												Hashtbl.remove available_workers worker_addr;
												return ()))))

let req_resp ?on_error worker_addr req process_resp =
	try_with
		(fun () ->
					Tcp.with_connection
						(Tcp.to_host_and_port (Host_and_port.host worker_addr) (Host_and_port.port worker_addr))
						(fun _s r w ->
									(Writer.write_sexp w (Request.sexp_of_t req);
										((Writer.flushed w)
											>>= (fun () -> Reader.read_sexp r))
										>>|
										(fun se ->
													match se with
													| `Ok se -> process_resp (Response.t_of_sexp se)
													| `Eof -> ()
										)))
		)
	>>| function
	| Ok () -> ()
	| Error exn ->
			match on_error with
			| Some handler -> handler exn
			| None -> printf "Ignored error for request %s : %s" (Sexp.to_string (Request.sexp_of_t req)) (Exn.to_string exn)

let ping_workers () =
	
	Hashtbl.iter available_workers
		~f:
		(fun ~key ~data ->
					ignore (
							( req_resp
									~on_error: (fun e ->
												printf "\nWorker %s  died - error  %s"
													(Host_and_port.to_string key) (Exn.to_string e);
												Hashtbl.remove available_workers key)
									key Request.Ping (fun se ->
												(match se with
													| Pong p ->
															assert (p = (Host_and_port.port key));
															(printf "\nWorker %s  pong "
																	(Host_and_port.to_string key);
																data.last_active <- Time.now ()
															)
													| _ -> failwith "Illegal response to Ping"
												)))))

let update_worker_status w status =
	match Hashtbl.find available_workers w with
	| Some worker ->
			worker.status <- status;
			worker.last_active <- Time.now ();
			true
	| None -> false

let rec init_workers () =
	Pipe.read free_workers_reader
	>>= (function
		| `Ok w ->
				req_resp
					~on_error: (fun e -> printf "\nError while initilizing worker %s : \n%s " (Host_and_port.to_string w) (Exn.to_string e))
					w
					(Request.Init (Worker_type.Mapper, "reader"))
					( fun resp ->
								match resp with
								| Ready t -> if t = Worker_type.Mapper then
											(
												printf "Mapper %s is ready" (Host_and_port.to_string w);
												
												if update_worker_status w Initialized_mapper then
													ignore (Pipe.write mapper_workers_writer w)
												
											)
										else printf "Invalid reply to init from worker %s, worker type mismatch" (Host_and_port.to_string w)
								| _ -> printf "Invalid reply to init from worker %s" (Host_and_port.to_string w)
					)
				>>= (fun () -> init_workers ())
		
		| `Eof -> return ()
	)

let get_ctl () =
	match !ctl with
	| Some m -> m
	| None -> failwith "No controller module loaded"

let do_mapping () =
	let ctl = get_ctl () in
	let rec map piece () =
		Pipe.read mapper_workers_reader
		>>= function
		| `Ok w ->
				let send (k, v) =
					if Hashtbl.mem map_results k then
						return ()
					else
						begin
							Hash_set.add sent_map_pieces k;
							ignore(update_worker_status w Working);
							req_resp w (Request.Map (k, v)) ( fun resp ->
											match resp with
											
											| Map (key, data) ->
											
													if update_worker_status w Initialized_mapper then
														ignore (Pipe.write mapper_workers_writer w); (* return back to queue *)
													
													if not (Hashtbl.mem map_results key) then
														ignore (Hashtbl.add map_results ~key ~data);
														
													Hash_set.remove sent_map_pieces key;
											
											| _ -> failwith "Invalid response type"
								)
						end
				in
				begin
					match piece with
					| Some piece -> send piece >>= map None
					| None ->
							let module C = (val ctl: Ifc.Controlling) in
							match C.next_piece () with
							| Some p -> send p >>= map None
							| None -> return ()
				end
		
		|`Eof -> return ()
	
	in map None ()

let run port =
	ctl:= Some (module Dummy_c.M: Ifc.Controlling);
	start port
	>>| (fun _s -> printf "\nController is running on port %d" port)
	>>| (fun () -> Deferred.forever () (fun () ->
								after (Time.Span.create ~sec: 10 ())
								>>| ping_workers ))
	>>= init_workers
	>>= never

let cmd =
	Command.async_basic ~summary: "Map Reduce controll server"
		(let open Command.Spec
			in
			empty +>
			(flag "-port" (optional_with_default 8765 int)
					~doc: " Port to listen on (default 8765)"))
		(fun port () -> run port)

let () = Command.run ~version: "0.1" cmd
