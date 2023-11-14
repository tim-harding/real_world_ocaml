open Base
open Stdio

module BasicColor = struct
  type t = Black | Red | Green | Yellow | Blue | Magenta | Cyan | White

  let to_int t =
    match t with
    | Black -> 0
    | Red -> 1
    | Green -> 2
    | Yellow -> 3
    | Blue -> 4
    | Magenta -> 5
    | Cyan -> 6
    | White -> 7
end

module Weight = struct
  type t = Regular | Bold

  let to_int t = match t with Regular -> 0 | Bold -> 8
end

module Rgb : sig
  type t

  val from_ints : int -> int -> int -> t option
  val to_int : t -> int
end = struct
  type max_six = int

  let make_max_six n = if n <= 5 then Some n else None

  type t = { r : max_six; g : max_six; b : max_six }

  let from_ints r g b =
    match make_max_six r with
    | None -> None
    | Some r -> (
        match make_max_six g with
        | None -> None
        | Some g -> (
            match make_max_six b with
            | None -> None
            | Some b -> Some { r; g; b }))

  let to_int { r; g; b } = 16 + (r * 36) + (g * 6) + b
end

module Grayscale : sig
  type t

  val from_int : int -> t option
  val to_int : t -> int
end = struct
  type t = int

  let from_int n = if n <= 24 then Some n else None
  let to_int t = t + 232
end

module Color = struct
  type t =
    | Basic of BasicColor.t * Weight.t
    | Rgb of Rgb.t
    | Grayscale of Grayscale.t

  let to_int t =
    match t with
    | Basic (color, weight) -> BasicColor.to_int color + Weight.to_int weight
    | Rgb rgb -> Rgb.to_int rgb
    | Grayscale grayscale -> Grayscale.to_int grayscale
end

let print_colored text color =
  printf "\027[38;5;%dm%s\027[0m" (Color.to_int color) text

(*
    Recursive data types example with boolean computation.
    'a specifies what goes under the Base tag. 
*)
type 'a expr =
  | Base of 'a
  | Const of bool
  | And of 'a expr list
  | Or of 'a expr list
  | Not of 'a expr

let rec _eval expr base_eval =
  let eval' expr = _eval expr base_eval in
  match expr with
  | Base base -> base_eval base
  | Const bool -> bool
  | And exprs -> List.for_all exprs ~f:eval'
  | Or exprs -> List.exists exprs ~f:eval'
  | Not expr -> not (eval' expr)

let _simplify expr =
  match expr with
  | (Base _ | Const _) as x -> x
  | And exprs -> (
      if
        (* If anything is false, so to is the entire expression *)
        List.exists exprs ~f:(function Const false -> true | _ -> false)
      then Const false
      else
        match
          (* Remove any true constants as they do not affect the result *)
          List.filter exprs ~f:(function Const true -> false | _ -> true)
        with
        | [] -> Const true
        | [ x ] -> x
        | exprs -> And exprs)
  | Or exprs -> (
      if List.exists exprs ~f:(function Const true -> true | _ -> false) then
        Const true
      else
        match
          List.filter exprs ~f:(function Const false -> false | _ -> true)
        with
        | [] -> Const false
        | [ x ] -> x
        | exprs -> Or exprs)
  (* This example demonstrates that handling all cases explicitly is valuable,
     since this code does not identify the opportunity to simplify
     Not (Not expr) -> expr *)
  | Not expr -> ( match expr with Const b -> Const (not b) | expr -> Not expr)

(* Implicit variant types are possible with the backtick *)
let _polymorphic_variants () =
  let three = `Int 3 in
  let four = `Four 4 in
  let nan = `Not_a_number in
  let _list = [ three; four; nan ] in
  ()

(* [< `Float of float | `Int of int ]
   indicates that the function can handle types with no more than these variants,
   but it can understand types with fewer.
   > indicates the opposite, that we must have at least certain variants.
   This can happen when a _ catch-all case is used in the pattern.
   That can be dangerous, as things like passing `Floot when you mean `Float
   won't be caught by the compiler. *)
let _is_positive = function `Int x -> x > 0 | `Float x -> Float.(x > 0.)

let run () =
  print_colored "Hello" (Color.Basic (BasicColor.Yellow, Weight.Regular));
  print_colored " world!\n" (Color.Basic (BasicColor.Green, Weight.Bold));
  Grayscale.from_int 12
  |> Option.iter ~f:(fun g -> print_colored "Gray" (Color.Grayscale g));
  Grayscale.from_int 18
  |> Option.iter ~f:(fun g -> print_colored "scale\n" (Color.Grayscale g));
  Rgb.from_ints 5 2 2
  |> Option.iter ~f:(fun rgb -> print_colored "r" (Color.Rgb rgb));
  Rgb.from_ints 2 5 2
  |> Option.iter ~f:(fun rgb -> print_colored "g" (Color.Rgb rgb));
  Rgb.from_ints 2 2 5
  |> Option.iter ~f:(fun rgb -> print_colored "b\n" (Color.Rgb rgb))
