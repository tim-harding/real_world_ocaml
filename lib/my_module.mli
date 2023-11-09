open Base

type t
(** The module's main type *)

val init : string -> int -> t
(** Creates a new t *)

val say_hi_to : t -> unit
(** Says hello to the named individual *)
