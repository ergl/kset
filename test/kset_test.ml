(* TODO: Improve this test *)
let get_table =
  let open QCheck in
  Test.make
    ~name: "Kset_internal.table_from_key always returns the table string
    from a valid key"
    ~count: 1000
    (make (Gen.small_string ~gen:Gen.printable)) (
    fun str ->
      Kset_internal.table_from_key (Kset_internal.table str) = str
  )

let unchanged_test =
  let open QCheck in
  Test.make
    ~name: "Kset_internal.changed returns false for empty sets,
    true after add, remove or swap operations,
    and false again after a reset operation"
    ~count: 1000
    (pair Models.arbitrary_key Models.arbitrary_key) (
    fun (k, k') ->
      let st = Kset_internal.empty () in
      let empty_flag = Kset_internal.changed st in
      Kset_internal.add k st;
      let after_add = Kset_internal.changed st in
      Kset_internal.swap k k' st;
      let after_swap = Kset_internal.changed st in
      Kset_internal.remove k' st;
      let after_remove = Kset_internal.changed st in
      Kset_internal.reset st;
      let after_reset = Kset_internal.changed st in
      (not empty_flag) &&
      after_add &&
      after_swap &&
      after_remove &&
      (not after_reset)
  )

let positive_find_test =
  let open QCheck in
  Test.make
    ~name: "Kset_internal.find will always return Some k for added keys k"
    ~count: 1000
    (pair Models.arbitrary_key Models.arbitrary_storage) (
    fun (key, set) ->
      Kset_internal.add key set;
      Kset_internal.find key set = Some key
  )

let negative_member_test =
  let open QCheck in
  Test.make
    ~name: "Kset_internal.find will always return None for non-added keys"
    ~count: 1000
    Models.arbitrary_key (
    fun key ->
      let set = Kset_internal.empty () in
      Kset_internal.find key set = None
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

let subkey_identity_test =
  let open QCheck in
  Test.make
    ~name: "Subkeys on singleton sets return the empty list"
    ~count: 1000
    Models.arbitrary_key (
    fun key ->
      let st = Kset_internal.empty () in
      Kset_internal.add key st;
      Kset_internal.subkeys key st = []
  )

let subkey_exclusive_test =
  let open QCheck in
  Test.make
    ~name: "Subkeys result should not contain the caller key"
    ~count: 1000
    (pair Models.arbitrary_key_hierarchy Models.arbitrary_storage) (
    fun (key_h, st) ->
      let first = List.hd key_h in
      List.iter (fun k -> Kset_internal.add k st) key_h;
      let subkeys = Kset_internal.subkeys first st in
      not @@ List.mem first subkeys
  )

let subkey_bucket_test =
  let open QCheck in
  Test.make
    ~name: "Subkeys shouldn't cross key buckets"
    ~count: 1000
    Models.arbitrary_key_hierarchy (
    fun key_h ->
      let n =
        Random.self_init ();
        Random.int (List.length key_h - 1)
      in
      let _, (hd::tl as range) = split_at_nth n key_h in
      let st = Kset_internal.empty () in
      List.iter (fun k -> Kset_internal.add k st) range;
      let subkeys = Kset_internal.subkeys hd st in
      if Kset_internal.is_data hd then
        List.for_all Kset_internal.is_data subkeys
      else if Kset_internal.is_index hd then
        List.for_all Kset_internal.is_index subkeys
      else if Kset_internal.is_uindex hd then
        List.for_all Kset_internal.is_uindex subkeys
      else subkeys = tl
  )

let subkeys_bare_test =
  let open QCheck in
  Test.make
    ~name: "For empty sets, Kset_internal.subkeys will always return the entire hierarchy"
    ~count: 1000
    Models.arbitrary_key_hierarchy (
    fun key_h ->
      let st = Kset_internal.empty () in
      let head, tail = List.hd key_h, List.tl key_h in
      List.iter (fun k -> Kset_internal.add k st) key_h;
      Kset_internal.subkeys head st = tail
  )

let subkeys_subset_test =
  let open QCheck in
  Test.make
    ~name: "For random sets, Kset_internal.subkeys will always return a superset of the original hierarchy"
    ~count: 1000
    (pair Models.arbitrary_key_hierarchy Models.arbitrary_storage) (
    fun (key_h, st) ->
      let head, tail = List.hd key_h, List.tl key_h in
      List.iter (fun k -> Kset_internal.add k st) key_h;
      let subkeys = Kset_internal.subkeys head st in
      List.for_all (fun k -> List.mem k subkeys) tail
  )

let prev_key_test =
  let open QCheck in
  Test.make
    ~name: "Prev key on hierarchies returns the first superkey"
    ~count: 1000
    Models.arbitrary_key_hierarchy (
    fun key_h ->
      let st = Kset_internal.empty ()
      and n =
        Random.self_init ();
        (Random.int @@ List.length key_h - 1) + 1
      in
      let elt = List.nth key_h n in
      let _ = List.iter (fun k -> Kset_internal.add k st) key_h in
      Kset_internal.prev_key elt st = Some (List.nth key_h @@ n - 1)
  )

let next_key_test =
  let open QCheck in
  Test.make
    ~name: "Next key on hierarchies returns the first subkey"
    ~count: 1000
    Models.arbitrary_key_hierarchy (
    fun key_h ->
      let st = Kset_internal.empty ()
      and n =
        Random.self_init ();
        Random.int @@ List.length key_h - 1
      in
      let elt = List.nth key_h n in
      let _ = List.iter (fun k -> Kset_internal.add k st) key_h in
      Kset_internal.next_key elt st = Some (List.nth key_h @@ n + 1)
  )

let batch_test =
  let open QCheck in
  Test.make
    ~name: "Batch on hierarchies returns the entire hierarchy"
    ~count: 1000
    Models.arbitrary_key_hierarchy (
    fun key_h ->
      let st = Kset_internal.empty () in
      let first, last =
        List.hd key_h, List.nth key_h @@ List.length key_h - 1
      in
      List.iter (fun k -> Kset_internal.add k st) key_h;
      Kset_internal.batch first last st = key_h
  )

let batch_range_test =
  let open QCheck in
  Test.make
    ~name: "Batch on identity ranges should return only one key"
    ~count: 1000
    Models.arbitrary_key (
    fun key ->
      let st = Kset_internal.empty () in
      Kset_internal.add key st;
      Kset_internal.batch key key st = [key]
  )

let _ = QCheck_runner.run_tests_main [
    get_table
  ; unchanged_test
  ; positive_find_test
  ; negative_member_test
  ; subkey_identity_test
  ; subkey_exclusive_test
  ; subkey_bucket_test
  ; subkeys_bare_test
  ; subkeys_subset_test
  ; prev_key_test
  ; next_key_test
  ; batch_test
  ; batch_range_test
  ]
