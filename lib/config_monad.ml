open Config
open Halt_error
open Memory
open Register_file
open Io_device

(* Monad, Applicative and Functor definitions *)

type ('a, 'io_device) m = 'io_device config ->  [`ok of ('a * 'io_device config) | `halt of halt_error]

let ( (>>=) : ('a, 'io_device) m -> ('a -> ('b, 'io_device) m) -> ('b, 'io_device) m) = fun ma mf ->
  fun c ->
  match ma c with
  | `halt e -> `halt e
  | `ok (a, c') -> mf a c'

let (pure : 'a -> ('a, 'io_device) m) = fun v -> fun c -> `ok (v,c)

let (let*) = (>>=)

let (>>) a b = a >>= fun () -> b

(* Monadic operations *)

let (get : (('io_device config), 'io_device) m) = fun c -> `ok(c,c)

let (gets : ('io_device config -> 'a) -> ('a, 'io_device) m) = fun f -> fun c -> `ok(f c,c)

let (set : ('io_device config) -> (unit, 'io_device) m) = fun c -> fun _ -> `ok((),c)

let (modify : (('io_device config) -> ('io_device config)) -> (unit, 'io_device) m) = fun f -> fun c -> `ok((),f c)

let (ok : (unit, 'io_device) m) = fun c -> `ok ((), c)

let (halt : halt_error -> ('a, 'io_device) m) = fun err -> fun _ -> `halt err

let (let@) a f = gets a >>= f

(* Getters *)

let rget r = gets (fun c -> register_get r c.r)
let mget a = gets (fun c -> memory_get a c.m)

(* Setters *)

let mset a w = modify (fun c -> {c with m = memory_set a w c.m})
let rset r w = modify (fun c -> {c with r = register_set c.layout r w c.r})
let rmodify r f = modify (fun c -> {c with r = register_set c.layout r (f (register_get r c.r)) c.r})
let set_backup b = modify (fun c -> {c with b})
let set_pc_old pc_old = modify (fun c -> {c with pc_old})
let set_io_state io_state = modify (fun c -> {c with io_state})
let raise_exception k = modify (exception_config k)
let increment_current_clock c = modify (fun c -> {c with current_clock = c.current_clock + 1}) c
let clear_registers c = modify (fun c -> {c with r = register_file_0}) c

let advance_config k =
  modify (fun c ->
    let io_state, current_clock, arrival_time =
      advance c.io_device k (c.io_state, c.current_clock, c.arrival_time)
    in { c with io_state; current_clock; arrival_time })