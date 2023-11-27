open Base
open Stdio

module type X_int = sig
  val x : int
end

(* Functor, a function from module to module *)
module Increment (M : X_int) : X_int = struct
  let x = M.x + 1
end

module Three = struct
  let x = 3
end

module Four = Increment (Three)

module Three_plus_plus = struct
  let x = 3
  let y = "Three"
end

(* Satisfies the interface *)
module Four_plus_plus = Increment (Three_plus_plus)

module type Comparable = sig
  type t

  val compare : t -> t -> int
end

(* Useful to define so that users can't construct their own t without using create *)
module type Interval_intf = sig
  type t
  type endpoint

  val create : endpoint -> endpoint -> t
  val is_empty : t -> bool
  val contains : t -> endpoint -> bool
  val intersect : t -> t -> t
end

(* Destructive substitution replaces uses of endpoint with int *)
module type Int_Interval_intf = Interval_intf with type endpoint := int

(* Now t can also derive sexp *)
module type Interval_intf_with_sexp = sig
  type t

  include Interval_intf with type t := t
  include Sexpable.S with type t := t
end

module Make_interval (Endpoint : sig
  type t

  include Comparable with type t := t
  include Sexpable.S with type t := t
end) :
  Interval_intf_with_sexp
  (* Sharing constraints needed so that when ints are passed to create,
     OCaml knows they're the same type. *)
    with type endpoint := Endpoint.t = struct
  type t = Interval of Endpoint.t * Endpoint.t | Empty [@@deriving sexp]

  let create low high =
    if Endpoint.compare low high > 0 then Empty else Interval (low, high)

  let is_empty = function Empty -> true | Interval _ -> false

  let contains t x =
    match t with
    | Empty -> false
    | Interval (l, h) -> Endpoint.compare x l >= 0 && Endpoint.compare x h <= 0

  let intersect t1 t2 =
    let min x y = if Endpoint.compare x y <= 0 then x else y in
    let max x y = if Endpoint.compare x y >= 0 then x else y in
    match (t1, t2) with
    | Empty, _ | _, Empty -> Empty
    | Interval (l1, h1), Interval (l2, h2) -> create (max l1 l2) (min h1 h2)

  let t_of_sexp sexp =
    match t_of_sexp sexp with Empty -> Empty | Interval (x, y) -> create x y
end

module Int_interval = Make_interval (Int)

module type S = sig
  type 'a t

  val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
end

module type Extension = sig
  type 'a t

  val iter : 'a t -> f:('a -> unit) -> unit
  val length : 'a t -> int
  val count : 'a t -> f:('a -> bool) -> int
  val for_all : 'a t -> f:('a -> bool) -> bool
  val exists : 'a t -> f:('a -> bool) -> bool
end

(* Easily add combinators to types with fold *)
module Extend (Arg : S) : Extension with type 'a t := 'a Arg.t = struct
  open Arg

  let iter t ~f = fold t ~init:() ~f:(fun () a -> f a)
  let length t = fold t ~init:0 ~f:(fun acc _ -> acc + 1)

  let count t ~f =
    fold t ~init:0 ~f:(fun count x -> count + if f x then 1 else 0)

  exception Short_circuit

  let for_all c ~f =
    try
      iter c ~f:(fun x -> if not (f x) then raise Short_circuit);
      true
    with Short_circuit -> false

  let exists c ~f =
    try
      iter c ~f:(fun x -> if f x then raise Short_circuit);
      false
    with Short_circuit -> true
end

module type Fqueue_intf = sig
  type 'a t

  val empty : 'a t
  val enqueue : 'a t -> 'a -> 'a t
  val dequeue : 'a t -> ('a * 'a t) option
  val fold : 'a t -> init:'acc -> f:('acc -> 'a -> 'acc) -> 'acc
end

module Fqueue_inner = struct
  type 'a t = 'a list * 'a list

  let empty = ([], [])
  let enqueue (in_list, out_list) x = (x :: in_list, out_list)

  let dequeue (in_list, out_list) =
    match out_list with
    | hd :: tl -> Some (hd, (in_list, tl))
    | [] -> (
        match List.rev in_list with
        | [] -> None
        | hd :: tl -> Some (hd, ([], tl)))

  let fold (in_list, out_list) ~init ~f =
    let after_out = List.fold ~init ~f out_list in
    List.fold_right ~init:after_out ~f:(fun x acc -> f acc x) in_list
end

(* TODO: Extend example in multiple files instead *)

module Fqueue = Extend (Fqueue_inner)

let run () =
  let open Int_interval in
  let a = create 5 10 in
  let b = create 7 15 in
  let combo = intersect a b in
  printf "%b, %b, %b, %b\n"
    (is_empty (create 10 5))
    (contains combo 8) (contains combo 13) (contains combo 6);
  ()
