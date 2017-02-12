type data =
  | I of Int32.t
  | F of Nativeint.t
  | S of string

let d_int x = I x
let d_float f = F f
let d_string s = S s

type key =
  | Bottom

  | KTable of string * key

  | KSPk of data * key
  | KCPk of data list * key

  | KField of string

  | KIndex of string * key
  | KIndexField of string * key
  | KIndexFieldValue of data * key
  | KIndexFieldKey of data

  | KUIndex of string * key
  | KUIndexField of string * key
  | KUIndexFieldValue of data
  (*[@@deriving show]*)

let table ~tname:t =
  KTable (t, Bottom)

let spk ~tname:t ~value:n =
  KTable (t, KSPk (n, Bottom))

let cpk ~tname:t ~value:nl =
  KTable (t, KCPk (nl, Bottom))

let field ~tname:t ~value:n ~fname:name =
  KTable (t, KSPk (n, KField name))

let index_key ~tname:t ~iname:i ~fname:f ~fvalue:v ~fkey:k =
  KTable (t, KIndex (i, KIndexField (f, KIndexFieldValue (v, KIndexFieldKey k))))

let uindex_key ~tname:t ~iname:i ~fname:f ~fvalue:v =
  KTable (t, KUIndex (i, KUIndexField (f, KUIndexFieldValue v)))

let prefix_separator = '%'
let prefix_separator_str = Char.escaped prefix_separator

let complex_key_separator = '&'
let complex_key_separator_str = Char.escaped complex_key_separator

let key_prefix = "PRIMARY"
let index_prefix = "INDEX"
let uindex_prefix = "UINDEX"

let concat (x::xs) =
  List.fold_left (fun s p -> s ^ prefix_separator_str ^ p) x xs

let encoding : string -> string = Encoding.d_string

let dencoding data = encoding @@ match data with
  | I x -> Encoding.d_int x
  | F f -> Encoding.d_float f
  | S s -> Encoding.d_string s

let create_complex_key nl =
  let rec c' v acc = function
    | [] -> acc
    | n::ns -> begin let enc = dencoding n in
        match v with
        | `First -> c' `Next (acc ^ enc) ns
        | _ -> c' `Next (acc ^ complex_key_separator_str ^ enc) ns
      end
  in c' `First "" nl

let append_complex_key : int -> string list -> string list = fun n (x::xs) ->
  (x ^ (string_of_int n |> encoding)) :: xs

let rec repr' (key : key) (acc : string list) : string list = match key with
  | KTable (t, ch) -> repr' ch @@ encoding t :: acc
  | KSPk (n, ch) -> repr' ch @@ (dencoding n) :: encoding key_prefix :: acc
  | KCPk (nl, ch) -> repr' ch @@ create_complex_key nl :: acc
  | KField s -> encoding s :: acc

  | KIndex (i, ch) -> repr' ch @@ encoding i :: encoding index_prefix :: acc
  | KIndexField (f, ch) -> repr' ch @@ encoding f :: acc
  | KIndexFieldValue (v, ch) -> repr' ch @@ dencoding v :: acc
  | KIndexFieldKey k -> dencoding k :: acc

  | KUIndex (i, ch) -> repr' ch @@ encoding i :: encoding uindex_prefix :: acc
  | KUIndexField (f, ch) -> repr' ch @@ encoding f :: acc
  | KUIndexFieldValue v -> dencoding v :: acc

  | Bottom -> acc

let repr k = repr' k []
             |> List.rev
             |> concat

type com_range = | Eq 
                 | Lt
                 | Gt

let to_comp = function
  | Eq -> 0
  | Lt -> -1
  | Gt -> 1

let v_compare x y = match Pervasives.compare x y with
  | 0 -> Eq
  | n -> if n < 0 then Lt else Gt

let rec key_compare left right = match left, right with
  | Bottom, Bottom -> Eq

  | KSPk (l, lch), KSPk (r, rch) -> compare_same_level (l, lch) (r, rch)
  | KCPk (l, lch), KCPk (r, rch) -> compare_same_level (l, lch) (r, rch)

  | KTable (l, lch), KTable (r, rch) |
    KIndex (l, lch), KIndex (r, rch) |
    KIndexField (l, lch), KIndexField (r, rch) |
    KUIndex (l, lch), KUIndex (r, rch) |
    KUIndexField (l, lch), KUIndexField (r, rch) -> compare_same_level (l, lch) (r, rch)

  | KIndexFieldValue (l, lch), KIndexFieldValue (r, rch) -> compare_same_level (l, lch) (r, rch)

  | KField l, KField r -> v_compare l r
  | KIndexFieldKey l, KIndexFieldKey r -> v_compare l r
  | KUIndexFieldValue l, KUIndexFieldValue r -> v_compare l r

  | KSPk _, KIndex _ |
    KSPk _, KUIndex _ |
    KIndex _, KUIndex _ -> Lt

  | KIndex _, KSPk _ |
    KUIndex _, KSPk _ |
    KUIndex _, KIndex _ -> Gt

  | KCPk _, KField _ |
    KCPk _, KIndexField _ |
    KCPk _, KIndexFieldValue _ |
    KCPk _, KIndexFieldKey _ |
    KCPk _, KUIndexField _ |
    KCPk _, KUIndexFieldValue _ -> Lt

  | KField _, KCPk _ |
    KIndexField _, KCPk _ |
    KIndexFieldValue _, KCPk _ |
    KIndexFieldKey _, KCPk _ |
    KUIndexField _, KCPk _ |
    KUIndexFieldValue _, KCPk _ -> Lt

  | Bottom, _ -> Lt
  | _, Bottom -> Gt

  | _, _ -> failwith "Wrong pair"
(*| _, _ -> failwith @@ "Wrong pair: " ^ (show_key left) ^ ", " ^ (show_key right)*)

and compare_same_level : type a. (a * key) -> (a * key) -> com_range =
  fun (l, lch) (r, rch) ->
    match v_compare l r with
    | Lt | Gt as r -> r
    | Eq -> key_compare lch rch

module Storage = Set.Make (struct
    type t = key
    let compare x y = key_compare x y |> to_comp
  end)

type t = Storage.t ref

let empty = ref Storage.empty

let wrap fn = try Some (fn ()) with Not_found -> None

let add elt t = t := (Storage.add elt !t)

let find x t = wrap @@ fun () -> Storage.find x !t

let next_key_opt elt t = match Storage.split elt !t with
  | (_, false, _) -> None
  | (_, true, gt) -> wrap @@ fun () -> Storage.min_elt gt

let next_key elt t = Js.Undefined.from_opt @@ next_key_opt elt t

let prev_key_opt elt t = match Storage.split elt !t with
  | (_, false, _) -> None
  | (lt, true, _) -> wrap @@ fun () -> Storage.max_elt lt

let prev_key elt t = Js.Undefined.from_opt @@ prev_key_opt elt t

let is_same_level = function
  (* Two table keys (metadata) *)
  | KTable (_, Bottom), KTable (_, Bottom)
  (* Two pk keys (sentinel) *)
  | KTable (_, KSPk (_, Bottom)), KTable (_, KSPk (_, Bottom))
  (* Two complex pk keys (sentinel) *)
  | KTable (_, KCPk (_, Bottom)), KTable (_, KCPk (_, Bottom))
  (* Two field keys (actual data) *)
  | KTable (_, KSPk (_, KField _)), KTable (_, KSPk (_, KField _))
  (* Two field keys with complex key (actual data) *)
  | KTable (_, KCPk (_, KField _)), KTable (_, KCPk (_, KField _))
  (* Two index field keys (just placeholder key that includes the data) *)
  | KTable (_, KIndex (_, KIndexField (_, KIndexFieldValue (_, KIndexFieldKey _)))), KTable (_, KIndex (_, KIndexField (_, KIndexFieldValue (_, KIndexFieldKey _))))
  (* Two unique index field keys (points to the sentinel key of the row) *)
  | KTable (_, KUIndex (_, KUIndexField (_, KUIndexFieldValue _))), KTable (_, KUIndex (_, KUIndexField (_, KUIndexFieldValue _))) -> true
  | _, _ -> false

let is_subkey l r = match key_compare l r with
  | Eq | Gt -> false
  | Lt -> not @@ is_same_level (l, r)

let valid_range l r = match key_compare l r with
  | Gt -> false
  | _ -> true

let collect_while ini fn t =
  let rec collect acc = function
    | None -> acc
    | Some s -> begin match fn s with
        | true -> collect (s :: acc) (next_key_opt s t)
        | false -> collect acc None
      end
  in
  match find ini t with 
  | None -> []
  | Some s -> List.rev @@ collect [s] (next_key_opt s t)

let subkeys ini t =
  let still_subkey = is_subkey ini in
  collect_while ini still_subkey t

let vbatch ini fin =
  let in_range a = valid_range a fin in
  collect_while ini in_range

let batch ini fin = match valid_range ini fin with
  | false -> invalid_arg "Kset.batch"
  | true -> vbatch ini fin
