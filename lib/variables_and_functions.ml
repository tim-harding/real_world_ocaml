open Base
open Stdio

let variables_and_functions () =
  let languages = "OCaml,Perl,C++,C" in
  let languages =
    (* This variable shadows and is only scoped inside the in binding *)
    let languages = String.split languages ~on:',' in
    String.concat ~sep:"-" languages
  in
  printf "%s\n" languages;

  (* Patterns! *)
  let _ints, _strings = List.unzip [ (1, "one"); (2, "two"); (3, "three") ] in

  (* Handling irrefutable cases *)
  let _upcase_first_entry line =
    match String.split ~on:',' line with
    | [] -> assert false
    | hd :: tl -> String.concat ~sep:"," (String.uppercase hd :: tl)
  in

  (* First-class functions *)
  let transforms = [ String.uppercase; String.lowercase ] in
  let transformed = List.map transforms ~f:(fun tx -> tx "Hello") in
  List.iter transformed ~f:(fun s -> print_endline s);

  (* This type is int -> int -> int = <fun> *)
  (* or with a right-associative annotation, *)
  (* val abs_diff: int -> (int -> int) *)
  (* Which makes more sense in this curried format: *)
  (* let _abs_diff = (fun x -> (fun y -> abs (x - y))) *)
  let abs_diff x y = abs (x - y) in

  (* Currying! *)
  (* int * int -> int = <fun> *)
  let dist_from_3 = abs_diff 3 in
  printf "%d\n" (dist_from_3 8);

  (* Recursion *)
  let rec _find_first_repeat list =
    match list with
    (* or-pattern *)
    | [] | [ _ ] -> None
    | x :: y :: tl -> if x = y then Some x else _find_first_repeat (y :: tl)
  in

  (* Reverse application operator *)
  let path = "/usr/bin:/usr/local/bin:/bin:/sbin" in
  String.split ~on:':' path
  |> List.dedup_and_sort ~compare:String.compare
  |> List.iter ~f:print_endline;

  (* as compared to *)
  let path = String.split ~on:':' path in
  let path = List.dedup_and_sort ~compare:String.compare path in
  List.iter ~f:print_endline path;

  (* application operator *)
  (* We want this... *)
  let _apply x =
    let open Float in
    sqrt @@ square @@ cos x
  in
  (* ...instead of this *)
  let _apply x =
    let open Float in
    sqrt (square (cos x))
  in

  (* function *)
  (* Shorthand for pattern matching on the last argument *)
  let some_or_default default = function Some x -> x | None -> default in
  List.iter
    ~f:(fun x -> printf "%d\n" x)
    (List.map ~f:(some_or_default 100) [ Some 3; None; Some 4 ]);

  (* Labeled arguments *)
  let ratio ~num ~denom = Float.of_int num /. Float.of_int denom in

  (* Punning *)
  let num = 10 in
  let denom = 5 in
  printf "%F\n" (ratio ~num ~denom);

  (* Higher-ordering functions and labels *)
  let apply_to_tuple f (first, second) = f ~first ~second in
  let _apply_to_tuple_2 f (first, second) = f ~second ~first in
  let divide ~first ~second = first / second in
  (*
     This version does not compile:
     apply_to_tuple_2 divide (3, 4)
     Because of
     got      first:int -> second:int -> int
     expected second:'a -> first:'b   -> 'c
  *)
  let _ = apply_to_tuple divide (3, 4) in

  (* Optional arguments *)
  let concat ?(sep = "") x y = x ^ sep ^ y in
  printf "%s\n" (concat "Hello" "world" ~sep:" ");
  (* Can pass in an Option to optional argument explicitly *)
  let uppercase_concat ?sep a b = concat ?sep (String.uppercase a) b in
  printf "%s\n" (uppercase_concat "foo" "bar");
  printf "%s\n" (uppercase_concat "foo" "bar" ~sep:":");

  (* Labeled argument inference *)
  (*
     Since we change the order of the named arguments to f 
     and the named arguments are part of the type,
     we need to specify the type in the declaration
  *)
  let _numeric_deriv ~delta ~x ~y ~(f : x:float -> y:float -> float) =
    let x' = x +. delta in
    let y' = y +. delta in
    let base = f ~x ~y in
    let dx = (f ~y ~x:x' -. base) /. delta in
    let dy = (f ~x ~y:y' -. base) /. delta in
    (dx, dy)
  in

  (* Optional arguments and partial application *)
  (*
     An optional argument is erased as soon as the first positional
     argument defined after the optional argument is passed in.
     Therefore, the ~sep argument is erased in this example.
  *)
  let _colon_concat = concat "# " in

  ()
