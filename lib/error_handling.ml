open Base
open Stdio

let _fail () = Error.of_string "Failure"
let float_from_string s = Or_error.try_with (fun () -> Float.of_string s)

let print_result = function
  | Ok _ -> print_endline "Ok"
  (* TODO: I want to pretty-print this error, but I can't figure it out *)
  | Error _ -> print_endline "Error"

let _s_expression_error = Error.create "Unexpected character" 'c' Char.sexp_of_t

let _s_expression_error =
  Error.t_of_sexp [%sexp "List is too long", [ 1; 2; 3 ]]

let _s_expression_error =
  let a = "foo" in
  let b = ("foo", [ 3; 4 ]) in
  Or_error.error_s
    [%message "Something went wrong" (a : string) (b : string * int list)]

let _error_context () =
  Error.tag
    (Error.of_list
       [
         Error.of_string "Your tires were slashed";
         Error.of_string "Your windshield was smashed";
       ])
    ~tag:"Over the weekend"

(* Plain *)
let _compute_bounds ~compare list =
  let sorted = List.sort ~compare list in
  match (List.hd sorted, List.tl sorted) with
  | None, _ | _, None -> None
  | Some x, Some y -> Some (x, y)

(* Option.bind *)
let _compute_bounds ~compare list =
  let open Option.Monad_infix in
  let sorted = List.sort ~compare list in
  List.hd sorted >>= fun first ->
  List.last sorted >>= fun last -> Some (first, last)

(* Let syntax *)
let _compute_bounds ~compare list =
  let open Option.Let_syntax in
  let sorted = List.sort ~compare list in
  let%bind first = List.hd sorted in
  let%bind last = List.tl sorted in
  Some (first, last)

(* Option.both *)
let _compute_bounds ~compare list =
  let sorted = List.sort ~compare list in
  Option.both (List.hd sorted) (List.tl sorted)

(* Exceptions are ordinary values of type exn *)
(* The derive helps the error message be more informational *)
(* More utilities for exceptions in Common and Exn *)
exception Key_not_found of string [@@deriving sexp]

let _exceptions = [ Division_by_zero; Key_not_found "b" ]
let throw_exn () = raise (Key_not_found "stuff")
let parse_line line = String.split ~on:',' line |> List.map ~f:Float.of_string

let load filename =
  let inc = In_channel.create filename in
  Exn.protect
    ~f:(fun () -> In_channel.input_lines inc |> List.map ~f:parse_line)
    ~finally:(fun () -> In_channel.close inc)

let _match_exceptions () =
  match Float.of_string "a.bc" with
  | n -> printf "Got %f\n" n
  | exception _ -> print_endline "Exception"

let run () =
  float_from_string "3.14" |> print_result;
  float_from_string "a.bc" |> print_result;
  try throw_exn () with
  | Key_not_found msg -> print_endline msg
  (* Assertions capture the source location of the exception *)
  | _ -> assert false
