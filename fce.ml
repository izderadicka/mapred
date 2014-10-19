open Protocol

module type IMapper =
	sig
		val map: string -> Data.t -> (string * Data.t) list
	end

module Mapper(M:Ifc.Mapping): IMapper =
	struct
	let map key data =
		let data' = M.read_data data in
		M.map M.write_data key data'
	end