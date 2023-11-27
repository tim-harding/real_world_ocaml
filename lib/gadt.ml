open Core

(* With normal variants: *)
type _value = Int of int | Bool of bool

type _expr =
  | Value of _value
  | Eq of _expr * _expr
  | Plus of _expr * _expr
  | If of _expr * _expr * _expr

exception Ill_typed

let rec eval expr =
  match expr with
  | Value v -> v
  | If (c, t, e) -> (
      match eval c with
      | Bool b -> if b then eval t else eval e
      | Int _ -> raise Ill_typed)
  | Eq (x, y) -> (
      match (eval x, eval y) with
      | Bool _, _ | _, Bool _ -> raise Ill_typed
      | Int f1, Int f2 -> Bool (f1 = f2))
  | Plus (x, y) -> (
      match (eval x, eval y) with
      | Bool _, _ | _, Bool _ -> raise Ill_typed
      | Int f1, Int f2 -> Int (f1 + f2))

module type Typesafe_lang_sig = sig
  type 'a t

  val int : int -> int t
  val bool : bool -> bool t
  val if_ : bool t -> 'a t -> 'a t -> 'a t
  val eq : int t -> int t -> bool t
  val plus : int t -> int t -> int t
  val int_eval : int t -> int
  val bool_eval : bool t -> bool
end

module Typesafe_lang : Typesafe_lang_sig = struct
  (* 'a is a phantom type because it doesn't show up in the body of t.
     Therefore, it is free to take on any type. *)
  type 'a t = _expr

  let int x = Value (Int x)
  let bool x = Value (Bool x)
  let if_ c t e = If (c, t, e)
  let eq x y = Eq (x, y)
  let plus x y = Plus (x, y)

  let int_eval expr =
    match eval expr with Int x -> x | Bool _ -> raise Ill_typed

  let bool_eval expr =
    match eval expr with Bool x -> x | Int _ -> raise Ill_typed
end

(* With GADTs: *)
type _ value = Int : int -> int value | Bool : bool -> bool value

type _ expr =
  | Value : 'a value -> 'a expr
  | Eq : int expr * int expr -> bool expr
  | Plus : int expr * int expr -> int expr
  | If : bool expr * 'a expr * 'a expr -> 'a expr

(* Alternate style of type signature *)
let _eval_value (type a) (v : a value) : a =
  match v with Int x -> x | Bool x -> x

let eval_value : type a. a value -> a = function Int x -> x | Bool x -> x

(* This style of type signature is a bit more complicated for recursive functions *)
let rec _eval : 'a. 'a expr -> 'a =
 fun (type a) (x : a expr) ->
  match x with
  | Value v -> eval_value v
  | If (c, t, e) -> if _eval c then _eval t else _eval e
  | Eq (x, y) -> _eval x = _eval y
  | Plus (x, y) -> _eval x + _eval y

let rec eval : type a. a expr -> a = function
  | Value v -> eval_value v
  | If (c, t, e) -> if eval c then eval t else eval e
  | Eq (x, y) -> eval x = eval y
  | Plus (x, y) -> eval x + eval y

(* Varying your return type *)
module If_not_found = struct
  type (_, _) t =
    | Raise : ('a, 'a) t
    | Return_none : ('a, 'a option) t
    | Default_to : 'a -> ('a, 'a) t
end

let rec _flexible_find :
    type a b. f:(a -> bool) -> a list -> (a, b) If_not_found.t -> b =
 fun ~f list if_not_found ->
  match list with
  | [] -> (
      match if_not_found with
      | Raise -> failwith "No matching item found"
      | Return_none -> None
      | Default_to x -> x)
  | hd :: tl ->
      if f hd then
        match if_not_found with
        | Raise -> hd
        | Return_none -> Some hd
        | Default_to _ -> hd
      else _flexible_find ~f tl if_not_found

type stringable =
  | Stringable : { value : 'a; to_string : 'a -> string } -> stringable

(* Abstracting computational machines / Combinators *)
type (_, _) pipeline =
  | Step : ('a -> 'b) * ('b, 'c) pipeline -> ('a, 'c) pipeline
  | Empty : ('a, 'a) pipeline

let ( @> ) f pipeline = Step (f, pipeline)

let rec exec : type a b. (a, b) pipeline -> a -> b =
 fun pipeline input ->
  match pipeline with Empty -> input | Step (f, tail) -> exec tail (f input)

(* Similar to |> but with extra capabilities *)
let exec_with_profile pipeline input =
  let rec loop :
      type a b.
      (a, b) pipeline -> a -> Time_ns.Span.t list -> b * Time_ns.Span.t list =
   fun pipeline input rev_profile ->
    match pipeline with
    | Empty -> (input, rev_profile)
    | Step (f, tail) ->
        let start = Time_ns.now () in
        let output = f input in
        let elapsed = Time_ns.diff (Time_ns.now ()) start in
        loop tail output (elapsed :: rev_profile)
  in
  let output, rev_profile = loop pipeline input [] in
  (output, List.rev rev_profile)

(* Narrowing the possibilities *)
type incomplete = Incomplete
type complete = Complete

type (_, _) coption =
  | Absent : (_, incomplete) coption
  | Present : 'a -> ('a, _) coption

(* When coption is known to be complete, pattern matching is narrowed to just Present *)
let _get (o : (_, complete) coption) = match o with Present x -> x

(* ...or this way: *)
let _get (Present x : (_, complete) coption) = x

(* 'c indicates whether it is complete,
   at which point user_id and permission must both be complete *)
type 'c logon_request = {
  user_name : string;
  user_id : (int, 'c) coption;
  permission : (bool, 'c) coption;
}

let _set_user_id request x = { request with user_id = Present x }
let _set_permission request x = { request with permission = Present x }

let _maybe_complete request : complete logon_request option =
  match (request.user_id, request.permission) with
  | Absent, _ | _, Absent -> None
  | (Present _ as user_id), (Present _ as permission) ->
      Some { request with user_id; permission }

(* Narrowing without GADTs *)
let _print_result (x : (int, Nothing.t) Result.t) =
  (* The dot is the refutation case that says it cannot be reached *)
  match x with Ok x -> printf "%d\n" x | Error _ -> .

let _print_result (x : (int, Nothing.t) Result.t) =
  (* Sometimes the refutation can be elided *)
  match x with Ok x -> printf "%d\n" x

let run () =
  (* Existential types *)
  let print_stringable (Stringable s) = print_endline (s.to_string s.value) in
  let stringables =
    let s value to_string = Stringable { to_string; value } in
    [ s 100 Int.to_string; s 12.3 Float.to_string; s "foo" Fn.id ]
  in
  List.iter ~f:print_stringable stringables;

  ()
