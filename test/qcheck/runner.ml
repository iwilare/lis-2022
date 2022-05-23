QCheck_runner.run_tests ~colors:true ~verbose:true
  (
    (*Registers_test.tests @
    Memory_test.tests @
    Semantics_test.tests @
    Instructions_test.tests @
    Security_test.tests @*)
    Enclave_map_test.tests
  )
