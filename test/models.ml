open QCheck

let gen_str = Gen.small_string ~gen:Gen.printable
and gen_ui32 = Gen.ui32
and gen_nat = Gen.nat

let table_keys = Gen.map (fun x -> Kset_internal.table x) gen_str

let primary_keys =
  [ Gen.map
      (fun (x, y) -> Kset_internal.spk x (Kset_internal.d_int y))
      Gen.(pair gen_str gen_ui32)
  ; Gen.map
      (fun (x, y) -> Kset_internal.spk x (Kset_internal.d_string y))
      Gen.(pair gen_str gen_str)
  ]

let field_keys =
  [ Gen.map
      (fun (x, y, z) -> Kset_internal.field x (Kset_internal.d_int y) z)
      Gen.(triple gen_str gen_ui32 gen_str)
  ; Gen.map
      (fun (x, y, z) -> Kset_internal.field x (Kset_internal.d_string y) z)
      Gen.(triple gen_str gen_str gen_str)
  ]

let index_keys =
  [ Gen.map
      (fun ((t, i, f, fv), fk) ->
         Kset_internal.index_key t i f (Kset_internal.d_string fv) (Kset_internal.d_int fk))
      Gen.(pair (quad gen_str gen_str gen_str gen_str) gen_ui32)

  ; Gen.map
      (fun ((t, i, f, fv), fk) ->
         Kset_internal.index_key t i f (Kset_internal.d_string fv) (Kset_internal.d_string fk))
      Gen.(pair (quad gen_str gen_str gen_str gen_str) gen_str)
  ]

let uindex_keys =
  [ Gen.map
      (fun (t, i, f, fv) ->
         Kset_internal.uindex_key t i f (Kset_internal.d_string fv))
      Gen.(quad gen_str gen_str gen_str gen_str)

  ; Gen.map
      (fun (t, i, f, fv) ->
         Kset_internal.uindex_key t i f (Kset_internal.d_int fv))
      Gen.(quad gen_str gen_str gen_str gen_ui32)
  ]

let key_generator = Gen.frequency
    [ (1, table_keys)
    ; (2, Gen.oneof primary_keys)
    ; (4, Gen.oneof field_keys)
    ; (2, Gen.oneof index_keys)
    ; (2, Gen.oneof uindex_keys)
    ]

let key_printer = Kset_internal.show_key
let arbitrary_key = make ~print:key_printer key_generator

let table_hierarchy =
  Gen.map (fun (t,k,f) ->
    [ Kset_internal.table t
    ; Kset_internal.spk t (Kset_internal.d_int k)
    ; Kset_internal.field t (Kset_internal.d_int k) f
    ]) Gen.(triple gen_str gen_ui32 gen_str)

let index_hierarchy =
  Gen.map (fun ((t,i,f,fv),fk) ->
    [ Kset_internal.table t
    ; Kset_internal.raw_index t i
    ; Kset_internal.raw_index_field t i f
    ; Kset_internal.raw_index_field_value t i f (Kset_internal.d_string fv)
    ; Kset_internal.index_key t i f (Kset_internal.d_string fv) (Kset_internal.d_int fk)
    ]) Gen.(pair (quad gen_str gen_str gen_str gen_str) gen_ui32)

let uindex_hierarchy =
  Gen.map (fun (t,i,f,fv) ->
    [ Kset_internal.table t
    ; Kset_internal.raw_uindex t i
    ; Kset_internal.raw_uindex_field t i f
    ; Kset_internal.uindex_key t i f (Kset_internal.d_string fv)
    ]) Gen.(quad gen_str gen_str gen_str gen_str)

let mixed_hierarchy =
  Gen.map (fun ((t,i,f,fv),fk) ->
    [ Kset_internal.table t
    ; Kset_internal.spk t (Kset_internal.d_int fk)
    ; Kset_internal.field t (Kset_internal.d_int fk) f

    ; Kset_internal.raw_index t i
    ; Kset_internal.raw_index_field t i f
    ; Kset_internal.raw_index_field_value t i f (Kset_internal.d_string fv)
    ; Kset_internal.index_key t i f (Kset_internal.d_string fv) (Kset_internal.d_int fk)

    ; Kset_internal.raw_uindex t i
    ; Kset_internal.raw_uindex_field t i f
    ; Kset_internal.uindex_key t i f (Kset_internal.d_string fv)
    ]) Gen.(pair (quad gen_str gen_str gen_str gen_str) gen_ui32)

let hierarchy_generator = Gen.frequency
    [ (1, table_hierarchy)
    ; (2, index_hierarchy)
    ; (2, uindex_hierarchy)
    ; (4, mixed_hierarchy)
    ]

let key_h_printer l = List.fold_left (^) ""  @@ List.map Kset_internal.show_key l
let arbitrary_key_hierarchy = make
    ~print:key_h_printer
    ~small: (List.length)
    hierarchy_generator

let storage_generator =
  Gen.map
    (fun key_list ->
       let set = Kset_internal.empty () in
       List.iter (fun k -> Kset_internal.add k set) key_list;
       set)
    (Gen.list key_generator)

let storage_printer t = Kset_internal.contents t |> key_h_printer

let arbitrary_storage = make
    ~print:storage_printer
    ~small: (fun t -> Kset_internal.contents t |> List.length)
    storage_generator
