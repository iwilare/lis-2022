type halt_error = NoIn | NoOut | HaltUM | ExecutingEnclaveData

let string_of_halt_error = function
  | NoIn -> "NoIn"
  | NoOut -> "NoOut"
  | HaltUM -> "HaltUM"
  | ExecutingEnclaveData -> "ExecutingEnclaveData"