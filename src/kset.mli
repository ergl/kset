type key
type data

val d_int : Int32.t -> data
val d_float : Nativeint.t -> data
val d_string : string -> data

(* Create a key for table metadata *)
val table : tname:string -> key

(* Create a simple sentinel key *)
val spk : tname:string -> value:data -> key

(* Create a complex sentinel key *)
(*val cpk : tname:string -> value:data list -> key*)

(* Create a fiel key *)
val field : tname:string -> value:data -> fname:string -> key

(* An index key *)
val index_key : tname:string 
  -> iname:string
  -> fname:string
  -> fvalue:data
  -> fkey:data
  -> key

(* An unique index key *)
val uindex_key : tname:string
  -> iname:string
  -> fname:string
  -> fvalue:data
  -> key

val repr : key -> string

type t

val empty : t

val add : key -> t -> unit
val find : key -> t -> key Js.Undefined.t

val next_key : key -> t -> key Js.Undefined.t
val prev_key : key -> t -> key Js.Undefined.t

val subkeys : key -> t -> key list

val batch : key -> key -> t -> key list
