type halt_error = NoIn | NoOut | HaltUM | ExecutingEnclaveData | HaltPM | DecodeFail | MemoryViolation

let string_of_halt_error = function
  | NoIn -> "NoIn"
  | NoOut -> "NoOut"
  | HaltPM -> "HaltPM"
  | HaltUM -> "HaltUM"
  | ExecutingEnclaveData -> "ExecutingEnclaveData"
  | DecodeFail -> "DecodeFail"
  | MemoryViolation -> "MemoryViolation"
