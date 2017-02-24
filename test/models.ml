open QCheck

let gen_str = Gen.small_string ~gen:Gen.printable
and gen_ui32 = Gen.ui32
and gen_nat = Gen.nat

let table_keys =
  [ Gen.map (fun x -> Kset.table x) gen_str]

let primary_keys =
  [ Gen.map
      (fun (x, y) -> Kset.spk x (Kset.d_int y))
      Gen.(pair gen_str gen_ui32)
  ; Gen.map
      (fun (x, y) -> Kset.spk x (Kset.d_string y))
      Gen.(pair gen_str gen_str)
  ]

let field_keys =
  [ Gen.map
      (fun (x, y, z) -> Kset.field x (Kset.d_int y) z)
      Gen.(triple gen_str gen_ui32 gen_str)
  ; Gen.map
      (fun (x, y, z) -> Kset.field x (Kset.d_string y) z)
      Gen.(triple gen_str gen_str gen_str)
  ]

let index_keys =
  [ Gen.map
      (fun ((t, i, f, fv), fk) ->
         Kset.index_key t i f (Kset.d_string fv) (Kset.d_int fk))
      Gen.(pair (quad gen_str gen_str gen_str gen_str) gen_ui32)

  ; Gen.map
      (fun ((t, i, f, fv), fk) ->
         Kset.index_key t i f (Kset.d_string fv) (Kset.d_string fk))
      Gen.(pair (quad gen_str gen_str gen_str gen_str) gen_str)
  ]

let uindex_keys =
  [ Gen.map
      (fun (t, i, f, fv) ->
         Kset.uindex_key t i f (Kset.d_string fv))
      Gen.(quad gen_str gen_str gen_str gen_str)

  ; Gen.map
      (fun (t, i, f, fv) ->
         Kset.uindex_key t i f (Kset.d_int fv))
      Gen.(quad gen_str gen_str gen_str gen_ui32)
  ]

let key_generator = Gen.oneof
    ( table_keys
      @ primary_keys
      @ field_keys
      @ index_keys
      @ uindex_keys
    )

let key_printer = Kset.show_key
let arbitrary_key = make ~print:key_printer key_generator

let table_hierarchy =
  Gen.map (fun (t,k,f) ->
    [ Kset.table t
    ; Kset.spk t (Kset.d_int k)
    ; Kset.field t (Kset.d_int k) f
    ]) Gen.(triple gen_str gen_ui32 gen_str)

let index_hierarchy =
  Gen.map (fun ((t,i,f,fv),fk) ->
    [ Kset.table t
    ; Kset.raw_index t i
    ; Kset.raw_index_field t i f
    ; Kset.raw_index_field_value t i f (Kset.d_string fv)
    ; Kset.index_key t i f (Kset.d_string fv) (Kset.d_int fk)
    ]) Gen.(pair (quad gen_str gen_str gen_str gen_str) gen_ui32)

let uindex_hierarchy =
  Gen.map (fun (t,i,f,fv) ->
    [ Kset.table t
    ; Kset.raw_uindex t i
    ; Kset.raw_uindex_field t i f
    ; Kset.uindex_key t i f (Kset.d_string fv)
    ]) Gen.(quad gen_str gen_str gen_str gen_str)

let hierarchy_generator = Gen.oneof
    ( table_hierarchy
      :: index_hierarchy
      :: uindex_hierarchy
      :: []
    )

let key_h_printer l = List.fold_left (^) ""  @@ List.map Kset.show_key l
let arbitrary_key_hierarchy = make
    ~print:key_h_printer
    ~small: (List.length)
    hierarchy_generator

let storage_generator =
  Gen.map
    (fun key_list ->
       let set = Kset.empty () in
       List.iter (fun k -> Kset.add k set) key_list;
       set)
    (Gen.list key_generator)

let storage_printer t = Kset.contents t |> key_h_printer

let arbitrary_storage = make
    ~print:storage_printer
    ~small: (fun t -> Kset.contents t |> List.length)
    storage_generator
