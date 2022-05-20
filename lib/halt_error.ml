type exception_reason = HaltPM | DecodeFail | MemoryViolation

type halt_error = NoIn | NoOut | HaltUM | ExecutingEnclaveData | TooMuchTime | Exception of exception_reason * int

let string_of_exception_reason = function
  | HaltPM -> "HaltPM"
  | DecodeFail -> "DecodeFail"
  | MemoryViolation -> "MemoryViolation"

let string_of_halt_error = function
  | NoIn -> "NoIn"
  | NoOut -> "NoOut"
  | HaltUM -> "HaltUM"
  | ExecutingEnclaveData -> "ExecutingEnclaveData"
  | TooMuchTime -> "TooMuchTime"
  | Exception(e,k) -> "Exception(" ^ string_of_exception_reason e ^ "," ^ string_of_int k ^ ")"
