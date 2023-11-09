open Base
open Base.Poly
open Stdio

let print_list l =
  List.iter ~f:(fun x -> printf "%d " x) l;
  print_endline ""

let print_option o =
  match o with None -> print_endline "None" | Some x -> printf "Some %d\n" x

let reverse l = List.fold ~init:[] ~f:(fun acc hd -> hd :: acc) l

let max_widths header rows =
  let lengths l = List.map l ~f:(fun s -> String.length s) in
  List.fold rows ~init:(lengths header) ~f:(fun acc row ->
      List.map2_exn ~f:Int.max acc (lengths row))

let render_separator widths =
  let pieces = List.map widths ~f:(fun w -> String.make w '-') in
  (* ^ is string concatenation, less efficient because it allocates a new string each time *)
  "|-" ^ String.concat ~sep:"-+-" pieces ^ "-|"

let pad s length = s ^ String.make (length - String.length s) ' '

let render_row row widths =
  let padded = List.map2_exn row widths ~f:pad in
  "| " ^ String.concat ~sep:" |" padded ^ " |"

let render_table header rows =
  let widths = max_widths header rows in
  String.concat ~sep:"\n"
    (render_row header widths :: render_separator widths
    :: List.map rows ~f:(fun row -> render_row row widths))

let lists_and_patterns () =
  let rec drop_value l to_drop =
    match l with
    | [] -> []
    | hd :: tl ->
        let new_tl = drop_value tl to_drop in
        if hd = to_drop then new_tl else hd :: new_tl
  in
  drop_value [ 1; 2; 3 ] 2 |> print_list;

  List.map ~f:String.length [ "Hello"; "World!" ] |> print_list;
  List.map2_exn ~f:Int.max [ 1; 2; 3 ] [ 3; 2; 1 ] |> print_list;
  (* Exception if the lists are of unequal length *)
  List.fold ~init:0 ~f:( + ) [ 1; 2; 3; 4 ] |> printf "%d\n";
  (* Similar to fold but doesn't require init and returns an option if empty *)
  List.reduce ~f:( + ) [ 1; 2; 3; 4 ] |> print_option;
  List.filter ~f:(fun x -> x % 2 = 0) [ 1; 2; 3; 4; 5 ] |> print_list;
  List.filter_map
    ~f:(fun x -> if x % 2 = 1 then Some (x * 7) else None)
    [ 1; 2; 3; 4; 5 ]
  |> print_list;
  let trues, falses =
    List.partition_tf ~f:(fun x -> x % 2 = 0) [ 0; 1; 2; 3; 4; 5 ]
  in
  print_list trues;
  print_list falses;

  print_endline
    (render_table
       [ "language"; "architect"; "first release" ]
       [
         [ "Lisp"; "John McCarthy"; "1958" ];
         [ "C"; "Dennis Ritchie"; "1969" ];
         [ "ML"; "Robin Milner"; "1973" ];
         [ "OCaml"; "Xavier Leroy"; "1996" ];
       ]);

  (* Not tail-call optimized because the addition happens outside the recursive call *)
  let rec _length = function [] -> 0 | _ :: tl -> 1 + _length tl in

  let rec length l n = match l with [] -> n | _ :: tl -> length tl (n + 1) in
  length (List.init 10_000_000 ~f:(fun x -> x)) 0 |> printf "%d\n";

  let rec remove_sequential_duplicates = function
    | ([] | [ _ ]) as l -> l
    | first :: (second :: _ as tl) when first = second ->
        remove_sequential_duplicates tl
    | first :: tl -> first :: remove_sequential_duplicates tl
  in
  remove_sequential_duplicates [ 1; 2; 2; 3 ] |> print_list;

  ()
