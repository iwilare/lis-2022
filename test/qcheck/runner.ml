QCheck_runner.run_tests ~colors:true ~verbose:true
  (Registers_test.tests @ Memory_test.tests)
