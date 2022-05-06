open Lis2022.Ast
open Lis2022.Register_file
open Lis2022.Semantics
open Lis2022.Memory
open Lis2022.Configuration
open Lis2022.Io_device

let () =
let data = {
    enclave_start=50;
    enclave_end=150;
} in
let code = {
    enclave_start=151;
    enclave_end=250
}  in
let layout = {
    data;
    code;
    isr=2345
} in
let instr = [
    ADD(R3, R4);

] in
let conf = init_configuration true layout default_io_device (memory_init ()) () in
conf.r.r3 <- 1;
conf.r.r4 <- 3;
if List.fold_left (fun _ instr -> step conf instr) (Ok ()) instr |> Result.is_ok then
    Printf.printf "%d   %d\n" (conf.r.r3) (conf.r.r4)
else
    ()
