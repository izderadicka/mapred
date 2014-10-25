open Protocol

module type IMapper =
sig
	val map: string -> Data.t -> (string * Data.t) list
end

module type IReducer = 
sig
	val reduce: string -> Data.t list -> Data.t
end

module Mapper(M: Ifc.Mapping): IMapper =
struct
	let map key data =
		let data' = M.read_data data in
		M.map M.write_data key data'
end

module Reducer(R: Ifc.Reducing) =
struct
	let reduce key data =
		let res = R.reduce R.read_data  key data in
		 R.write_data res
end