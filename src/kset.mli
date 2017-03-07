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

(* Create a field key *)
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

(* A string representation of key. Used to interact with Antidote,
   as it demands keys to be strings. *)
val repr : key -> string

(* A key that represents actual data stored in Antidote.
   That is, primary and field keys. *)
val is_data : key -> bool

(* A key that represents an index key or any of its subkeys. *)
val is_index : key -> bool

(* A key that represents an unique index key or any of its subkeys. *)
val is_uindex : key -> bool

(* Given a leaf index key, extract the primary key. Will return undefined
   if the given key is not a leaf index. *)
val get_index_data : key -> data Js.Undefined.t

(* Given a leaf field key, extract the field name.
   Returns undefined if the key is not a leaf field. *)
val field_from_key : key -> string Js.Undefined.t

type t

(* Create an empty key set, with the [changed] flag set to false. *)
val empty : unit -> t

(* [changed t] will return the state of the [changed] flag for [t].
   The changed flag is set to true whenver an [add] operation is performed.*)
val changed : t -> bool

(* [reset t] will turn the [changed] flag back to [false]. *)
val reset : t -> unit

val add : key -> t -> unit
val find : key -> t -> key Js.Undefined.t

(* [swap k k' t] will swap key [k] for [k'] in [t], if it exists.
   If [k] is not an element of [t], nothing will happen. *)
val swap : key -> key -> t -> unit

val remove : key -> t -> unit

(* [next_key k t] will return the next key in the kset,
   or undefined if k is the last key. *)
val next_key : key -> t -> key Js.Undefined.t

(* [next_key k t] will return the previous key in the kset,
   or undefined if k is the first key. *)
val prev_key : key -> t -> key Js.Undefined.t

(* [subkeys k t] will return a list of all the keys that are
   subkeys of [k]. [k] itself is not included in the result. *)
val subkeys : key -> t -> key list

(* [batch k k' t] will return all the keys between k and k'.
   Both [k] and [k'] are included in the result. *)
val batch : key -> key -> t -> key list

(* [contents t] will dump all the keys in the set. *)
val contents : t -> key list
