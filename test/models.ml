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

let storage_generator =
  Gen.map
    (fun key_list ->
       let set = Kset.empty () in
       List.iter (fun k -> Kset.add k set) key_list;
       set)
    (Gen.list key_generator)
