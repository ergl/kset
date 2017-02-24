let member_test =
  let open QCheck in
  Test.make
    ~name: "Kset.find will always return Some k for added keys k"
    ~count: 1000
      Kset.add key set;
      Kset.find key set = Some key
  )

let no_member_test =
  let open QCheck in
  Test.make
    ~name: "Kset.find will always return None for non-added keys"
    ~count: 1000
    Models.arbitrary_key (
    fun key ->
      let set = Kset.empty () in
      Kset.find key set = None
  )

let _ =
  QCheck_runner.run_tests_main [member_test; no_member_test]
