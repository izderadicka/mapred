open Core.Std

open Async.Std

open Protocol

type worker_status =
	| Ready | Initialized | Working | Dead

type worker_remote =
	{ address : Host_and_port.t; mutable last_active : Time.t;
		mutable status : worker_status
	}

type task = Request.t * Time.t * int

let ctl = ref None

let available_workers = Host_and_port.Table.create ()
let free_workers_reader, free_workers_writer = Pipe.create ()
let ready_workers_reader, ready_workers_writer = Pipe.create ()

let sent_pieces : task String.Hash_queue.t = String.Hash_queue.create ()
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

let req_resp_no_wait ?on_error worker_addr req process_resp =
	match on_error with
	| Some s -> Deferred.don't_wait_for (req_resp ~on_error: s worker_addr req process_resp)
	| None -> Deferred.don't_wait_for (req_resp worker_addr req process_resp)

let ping_workers () =
	
	Hashtbl.iter available_workers
		~f:
		(fun ~key ~data ->
					req_resp_no_wait
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
									)))

let update_worker_status w status =
	match Hashtbl.find available_workers w with
	| Some worker ->
			worker.status <- status;
			worker.last_active <- Time.now ();
			true
	| None -> false

let rec init_workers worker_param =
	Pipe.read free_workers_reader
	>>= (function
		| `Ok w ->
		
				req_resp_no_wait
					~on_error: (fun e -> printf "\nError while initilizing worker %s : \n%s " (Host_and_port.to_string w) (Exn.to_string e))
					w
					(Request.Init worker_param)
					( fun resp ->
								match resp with
								| Ready ->
										printf "\nMapper %s is ready" (Host_and_port.to_string w);
										if update_worker_status w Initialized then
											ignore (Pipe.write ready_workers_writer w)
								| _ -> printf "\nInvalid reply to init from worker %s" (Host_and_port.to_string w)
					)
				; init_workers worker_param
		
		| `Eof -> return ()
	)

let get_ctl () =
	match !ctl with
	| Some m -> m
	| None -> failwith "No controller module loaded"

let max_retries = ref 3

let  do_task ?(attempt=0) name piece process_data =
		let send req worker =
			match worker with
			| `Ok w ->
					let k = match req with
						| Request.Map (k, _) -> k
						| Request.Reduce(k, _) -> k
						| _ -> failwith "Invalid request type"
					in
					let return_worker () =
						if update_worker_status w Initialized then
							ignore (Pipe.write ready_workers_writer w); (* return back to queue *)
					in
					let remove_sent key =
						ignore (String.Hash_queue.remove sent_pieces key);
						printf "\nReceived %s for piece %s from %s" name key (Host_and_port.to_string w)
					in
					if Hashtbl.mem map_results k then (
						ignore (String.Hash_queue.remove sent_pieces k);
						return_worker () )
					else
						begin
							
							ignore (String.Hash_queue.remove sent_pieces k);
							ignore (String.Hash_queue.enqueue sent_pieces k (req, Time.now (), attempt));
							ignore (update_worker_status w Working);
							printf "\nSending piece %s for %s to %s" k name (Host_and_port.to_string w);
							req_resp_no_wait
								~on_error: (fun exn ->
									(* TODO: consider more specific approach per exn     *)
									(* type?                                             *)
											printf "\nException while %s piece %s:\n%s" name k (Exn.to_string exn);
											return_worker ();
									
								)
								w
								req
								( fun resp ->
									
											match resp with
											| Map _ | Reduce _ ->
											
													return_worker ();
													let key = process_data resp in
													remove_sent key;
											
											| Error msg ->
													printf "\nError in remote task for piece %s:\n%s" k msg;
													return_worker ();
											
											| _ -> failwith "Invalid response type"
								)
						end
			|`Eof -> failwith "Mapping workers queue is closed, cannot continue"
		
		in
		Pipe.read ready_workers_reader >>| send piece
		
let rec do_tasks name next_piece process_data () =
		match next_piece () with
		| Some p -> do_task name p process_data >>= do_tasks name next_piece process_data
		| None -> printf "\nNo more pieces to read"; return ()
	


let next_mapping_piece () =
	let module C = (val (get_ctl ()): Ifc.Controlling) in
	match C.next_piece () with
	| Some (k, v) -> Some (Request.Map (k, v))
	| None -> None

let process_mapping_data resp =
	match resp with
	| Response.Map (key, data) ->
			if not (Hashtbl.mem map_results key) then
				ignore (Hashtbl.add map_results ~key ~data);
			key
	| _ -> assert false

let wait_for_task = Time.Span.of_sec 5.0

let rec wait_finish for_name process_result () =
	match String.Hash_queue.first sent_pieces with
	| Some (_, last_time, _) ->
			let wait_period = Time.diff (Time.now ()) last_time in
			if Time.Span.(wait_period >= wait_for_task) then
				match String.Hash_queue.dequeue sent_pieces with
				| Some (resent, sent_time, att) ->
						let key = match resent with
							| Map (k, _) -> k
							| Reduce(k, _) -> k
							| _ -> assert false
						in
						if att >= !max_retries then
							failwith (sprintf "Reached max retries %d for %s piece %s, error" !max_retries for_name key)
						else
							
							printf "\nResend piece %s from %s attempt %d" key (Time.to_string sent_time) (att+1);
							do_task ~attempt:(att+1) for_name resent process_result 
							>>= wait_finish for_name process_result
				| None -> return ()
			else
				let to_wait = Time.Span.( wait_for_task - wait_period) in
				printf "\nWaiting for %s" (Time.Span.to_string to_wait);
				after (to_wait) >>= wait_finish for_name process_result
	| None -> printf "\nFinished %s" for_name; return ()

let run port task params =
	ctl:= Some (module Dummy_c.M: Ifc.Controlling);
	let module C = (val get_ctl (): Ifc.Controlling) in
	C.init params;
	start port
	>>| (fun _s -> printf "\nController is running on port %d" port)
	(* use for debuging >>| (fun () -> Deferred.forever () (fun () -> after  *)
	(* (Time.Span.create ~sec: 10 ()) >>| ping_workers ))                    *)
	>>| (fun () -> init_workers task >>> ident )
	>>= do_tasks "mapping" next_mapping_piece process_mapping_data
	>>| (fun () -> printf "\nAll Mapping sent")
	>>= wait_finish "mapping" process_mapping_data

let sexp_arg = Command.Spec.Arg_type.create
		(fun s -> (Sexp.of_string s))

let cmd =
	Command.async_basic ~summary: "Map Reduce controll server"
		(let open Command.Spec
			in
			empty +>
			(flag "-port" (optional_with_default 8765 int)
					~doc: " Port to listen on (default 8765)")
			+> (flag "-task" (required string) ~doc: "path to directory with task plugins")
			+> (flag "-params" (optional sexp_arg) ~doc:"task initilazition params (as s expression)")
		)
		(fun port task params () -> run port task params)

let () = Command.run ~version: "0.1" cmd
