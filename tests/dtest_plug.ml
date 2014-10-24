open Dtest_ep

module M:PLUG =
	struct
		let hello () = "Hello, World!"
	end
	
	
let () = 
 p := Some (module M:PLUG)

