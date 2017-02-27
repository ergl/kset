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

(* Create a raw index key. Used only for batches and subkeys *)
val raw_index : tname:string -> iname:string -> key

(* Create a raw index field name key. Used only for batches and subkeys *)
val raw_index_field : tname:string
  -> iname:string
  -> fname:string
  -> key

(* Create a raw index field value key. Used only for batches and subkeys *)
val raw_index_field_value : tname:string
  -> iname:string
  -> fname:string
  -> fvalue:data
  -> key

(* An index key *)
val index_key : tname:string 
  -> iname:string
  -> fname:string
  -> fvalue:data
  -> fkey:data
  -> key

val raw_uindex : tname:string -> iname:string -> key

(* Create a raw index field name key. Used only for batches and subkeys *)
val raw_uindex_field : tname:string
  -> iname:string
  -> fname:string
  -> key

(* An unique index key *)
val uindex_key : tname:string
  -> iname:string
  -> fname:string
  -> fvalue:data
  -> key

val repr : key -> string

val is_data : key -> bool
val is_index : key -> bool
val is_uindex : key -> bool

type t

val empty : unit -> t

val add : key -> t -> unit
val find : key -> t -> key Js.Undefined.t

val next_key : key -> t -> key Js.Undefined.t
val prev_key : key -> t -> key Js.Undefined.t

val subkeys : key -> t -> key list

val batch : key -> key -> t -> key list

val contents : t -> key list
