open Base
open Stdio

type t = { name : string; age : int }

let init name age = { name; age }
let say_hi_to t = printf "Hello, %s with age %d\n" t.name t.age

(* Creates a submodule to newtype a string and prevent confusion *)
module Password : sig
  type t

  val _of_string : string -> t
  val _to_string : t -> string
end = struct
  type t = string

  let _of_string x = x
  let _to_string x = x
end

(* Possible to reuse an implementation *)
module type Id = sig
  type t

  val _of_string : string -> t
  val _to_string : t -> string
  val ( = ) : t -> t -> bool
end

module String_id = struct
  type t = string

  let _of_string x = x
  let _to_string x = x
  let ( = ) = String.( = )
end

module Username : Id = String_id
module Hostname : Id = String_id

type session_info = { user : Username.t; host : Hostname.t }

let _equals s1 s2 =
  Username.( = ) s1.user s2.user && Hostname.( = ) s1.host s2.host

(* Prefer local opens *)
let _average x y =
  let open Int64 in
  (x + y) / of_int 2

(* And a more lightweight open syntax: *)
let _average x y = Int64.((x + y) / of_int 2)

(* Also possible to rebind a module name: *)
let _average x y =
  let module I = Int64 in
  x I.( + ) y I.( / ) I.of_int 2

(* We can expand and reexport the contents of a module *)
module Interval = struct
  type t = Interval of int * int | Empty

  let _create low high = Interval (low, high)
  let _empty () = Empty
end

module Expanded_interval = struct
  (* For mli file, we would need to add `include module type of Interval` *)
  include Interval

  let _contains t x =
    match t with
    | Empty -> false
    | Interval (low, high) -> x >= low && x <= high
end
