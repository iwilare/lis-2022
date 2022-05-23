open Layout

(* CPU mode *)

type cpu_mode = PM | UM

let cpu_mode_of_address enc pc =
  match get_memory_type enc pc with
  | EnclaveCode _ -> Some PM
  | EnclaveData -> None
  | Unprotected -> Some UM
