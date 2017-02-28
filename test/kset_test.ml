let positive_find_test =
  let open QCheck in
  Test.make
    ~name: "Kset.find will always return Some k for added keys k"
    ~count: 1000
    (pair Models.arbitrary_key Models.arbitrary_storage) (
    fun (key, set) ->
      Kset.add key set;
      Kset.find key set = Some key
  )

let negative_member_test =
  let open QCheck in
  Test.make
    ~name: "Kset.find will always return None for non-added keys"
    ~count: 1000
    Models.arbitrary_key (
    fun key ->
      let set = Kset.empty () in
      Kset.find key set = None
  )

let split_at_nth n l =
  let rec split acc idx rest =
    if idx = n then (acc, rest)
    else begin
      let h,tl = (List.hd rest, List.tl rest) in
      split (h :: acc) (idx + 1) tl
    end
  in
  let a,b = split [] 0 l in
  (List.rev a, b)

let subkeys_bare_test =
  let open QCheck in
  Test.make
    ~name: "For empty sets, Kset.subkeys will always return the entire hierarchy"
    ~count: 1000
    Models.arbitrary_key_hierarchy (
    fun key_h ->
      let st = Kset.empty () in
      let n = Random.self_init (); Random.int (List.length key_h) in
      let (head, tail) = split_at_nth n key_h in
      let _ = List.iter (fun k -> Kset.add k st) key_h in
      Kset.subkeys (List.hd tail) st = tail
  )

let subkeys_subset_test =
  let open QCheck in
  Test.make
    ~name: "For random sets, Kset.subkeys will always return a superset of the original hierarchy"
    ~count: 1000
    (pair Models.arbitrary_key_hierarchy Models.arbitrary_storage) (
    fun (key_h, st) ->
      let n = Random.self_init (); Random.int (List.length key_h) in
      let (head, tail) = split_at_nth n key_h in
      let _ = List.iter (fun k -> Kset.add k st) key_h in
      let all_subkeys = Kset.subkeys (List.hd tail) st in
      List.for_all (fun k -> List.mem k all_subkeys) tail
  )

let prev_key_test =
  let open QCheck in
  Test.make
    ~name: "Prev key on hierarchies returns the first superkey"
    ~count: 1000
    Models.arbitrary_key_hierarchy (
    fun key_h ->
      let st = Kset.empty ()
      and n =
        Random.self_init ();
        (Random.int @@ List.length key_h - 1) + 1
      in
      let elt = List.nth key_h n in
      let _ = List.iter (fun k -> Kset.add k st) key_h in
      Kset.prev_key elt st = Some (List.nth key_h @@ n - 1)
  )

let next_key_test =
  let open QCheck in
  Test.make
    ~name: "Next key on hierarchies returns the first subkey"
    ~count: 1000
    Models.arbitrary_key_hierarchy (
    fun key_h ->
      let st = Kset.empty ()
      and n =
        Random.self_init ();
        Random.int @@ List.length key_h - 1
      in
      let elt = List.nth key_h n in
      let _ = List.iter (fun k -> Kset.add k st) key_h in
      Kset.next_key elt st = Some (List.nth key_h @@ n + 1)
  )

let batch_test =
  let open QCheck in
  Test.make
    ~name: "Batch on hierarchies returns the entire hierarchy"
    ~count: 1000
    Models.arbitrary_key_hierarchy (
    fun key_h ->
      let st = Kset.empty () in
      let first, last =
        List.hd key_h, List.nth key_h @@ List.length key_h - 1
      in
      List.iter (fun k -> Kset.add k st) key_h;
      Kset.batch first last st = key_h
  )

let batch_range_test =
  let open QCheck in
  Test.make
    ~name: "Batch on identity ranges should return only one key"
    ~count: 100
    Models.arbitrary_key (
    fun key ->
      let st = Kset.empty () in
      Kset.add key st;
      Kset.batch key key st = [key]
  )

let _ = QCheck_runner.run_tests_main [
    positive_find_test
  ; negative_member_test
  ; subkeys_bare_test
  ; subkeys_subset_test
  ; prev_key_test
  ; next_key_test
  ; batch_test
  ; batch_range_test
  ]
