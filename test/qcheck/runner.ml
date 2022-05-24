QCheck_runner.run_tests ~colors:true ~verbose:true
  (
    Memory_test.tests @
    Registers_test.tests @
    Memory_test.tests @
    Semantics_test.tests @
    Serialization_test.tests @
    Security_test.tests @
    Enclave_map_test.tests
  )
