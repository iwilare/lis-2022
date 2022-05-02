(*

Helper modules to generate relevant data structures and values for tests

NOTE: ALWAYS USE QCHECK2 instead of QCHECK when possible
 *)

module G = QCheck2.Gen

module Memory = struct
  open Lis2022.Memory

  let gen_valid_address = G.int_range 0 limit
  let gen_valid_byte = G.int_range 0 255
  (* since for now bytes are treated as normal integers, should avoid magic numbers *)

  let gen_random_memory = G.array_size (G.pure limit) gen_valid_byte
  (*completely random, probably need to fix some parts such as enclave *)
end

module Register = struct
  open Lis2022.Register_file

  let pick_sr_mask = G.oneofl [ mask_c; mask_gie; mask_n; mask_v; mask_z ]
end

module Configuration = struct end
module Device = struct end
