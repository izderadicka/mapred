open Core.Std
open Async.Std


let () =
	after (Time.Span.create ~sec:5 ())
	>>> (fun () -> printf "Done");
	never_returns (Scheduler.go ())