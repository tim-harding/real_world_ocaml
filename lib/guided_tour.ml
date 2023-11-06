open Base
open Stdio

type point2d = { x : float; y : float }
type circle = { center : point2d; radius : float }
type rect = { lower_left : point2d; upper_right : point2d }
type segment = { endpoint1 : point2d; endpoint2 : point2d }
type scene_element = Circle of circle | Rect of rect | Segment of segment

type running_sum = {
  mutable sum : float;
  mutable sum_sq : float;
  mutable samples : int;
}

let hello () =
  (* Basic math *)
  let _ = printf "%d\n" (3_000 + 4) in
  let _ = printf "%F\n" (3.5 +. 6.) in
  let square x = x ** 2 in
  let _ = printf "%d\n" (square 4) in

  (* This is equivalent... *)
  let _ratio x y = Float.O.of_int x /. Float.O.of_int y in
  (* ...to this *)
  let ratio x y =
    let open Float.O in
    of_int x / of_int y
  in
  let _ = printf "%F\n" (ratio 9 3) in

  (* Without type annotations... *)
  let _sum_if_true test first second =
    (if test first then first else 0) + if test second then second else 0
  in
  (* ...and with *)
  let sum_if_true (test : int -> bool) (first : int) (second : int) =
    (if test first then first else 0) + if test second then second else 0
  in
  let even x = x % 2 = 0 in
  let _ = printf "%d\n" (sum_if_true even 3 4) in
  let _ = printf "%d\n" (sum_if_true even 2 4) in

  (* Tuples *)
  let _a_tuple = (3, "three") in
  let a_tuple : int * string = (3, "three") in
  let _three_int, _three_string = a_tuple in

  (* Tuples in function arguments *)
  let distance (x1, y1) (x2, y2) =
    let x = x2 -. x1 in
    let y = y2 -. y1 in
    Float.sqrt ((x *. x) +. (y *. y))
  in
  let _ = printf "%F\n" (distance (1., 1.) (5., 4.)) in

  (* Lists *)
  let languages = [ "OCaml"; "Perl"; "C" ] in
  let languages = "French" :: "Spanish" :: languages in
  let languages = [ "Rust"; "Swift" ] @ languages in
  let _ = printf "%d\n" (List.length languages) in
  let best_language languages =
    match languages with hd :: _tl -> hd | [] -> ""
  in
  let _ = printf "%s\n" (best_language languages) in

  (* Recursion *)
  let rec sum l = match l with [] -> 0 | hd :: tl -> hd + sum tl in
  let rec remove_sequential_duplicates list =
    match list with
    | [] -> []
    | [ x ] -> [ x ]
    | first :: second :: tl ->
        if first = second then remove_sequential_duplicates (second :: tl)
        else first :: remove_sequential_duplicates (second :: tl)
  in
  let _ = printf "%d\n" (sum (remove_sequential_duplicates [ 1; 2; 2; 3 ])) in

  (* Options *)
  let _divide x y = if y = 0 then None else Some (x / y) in
  let downcase_extension filename =
    match String.rsplit2 filename ~on:'.' with
    | None -> filename
    | Some (base, ext) -> base ^ "." ^ String.lowercase ext
  in
  let _ = printf "%s\n" (downcase_extension "Hello_World.TXT") in

  (* Records *)
  let p : point2d = { x = 3.; y = -4. } in
  let _magnitude { x = x_pos; y = y_pos } =
    Float.sqrt ((x_pos **. 2.) +. (y_pos **. 2.))
  in
  let magnitude { x; y } = Float.sqrt ((x **. 2.) +. (y **. 2.)) in
  printf "%F\n" (magnitude p);
  let distance a b = magnitude { x = a.x -. b.x; y = a.y -. b.y } in
  printf "%F\n" (distance { x = 3.; y = 4. } { x = 2.; y = 8. });

  (* Variants *)
  let is_inside_scene_element point element =
    let open Float.O in
    match element with
    | Circle { center; radius } -> distance center point < radius
    | Rect { lower_left; upper_right } ->
        point.x > lower_left.x && point.y > lower_left.y
        && point.x < upper_right.x && point.y < upper_right.y
    | Segment _ -> false
  in
  let is_inside_scene point scene =
    List.exists scene ~f:(fun el -> is_inside_scene_element point el)
  in
  printf "%b\n"
    (is_inside_scene { x = 3.; y = 7. }
       [ Circle { center = { x = 4.; y = 4. }; radius = 0.5 } ]);
  printf "%b\n"
    (is_inside_scene { x = 3.; y = 7. }
       [ Circle { center = { x = 4.; y = 4. }; radius = 5. } ]);

  (* Arrays *)
  let numbers = [| 1; 2; 3; 4 |] in
  printf "%d\n" numbers.(2);

  (* Mutable record fields *)
  let mean rsum = rsum.sum /. Float.of_int rsum.samples in
  let stdev rsum =
    Float.sqrt ((rsum.sum_sq /. Float.of_int rsum.samples) -. (mean rsum **. 2.))
  in
  let create () = { sum = 0.; sum_sq = 0.; samples = 0 } in
  let update rsum x =
    rsum.samples <- rsum.samples + 1;
    rsum.sum <- rsum.sum +. x;
    rsum.sum_sq <- rsum.sum_sq +. (x **. 2.)
  in
  let rsum = create () in
  List.iter [ 1.; 3.; 2.; -7.; 4.; 5. ] ~f:(fun x -> update rsum x);
  printf "%F\n" (mean rsum);
  printf "%F\n" (stdev rsum);

  (* Refs *)
  let x = ref 0 in
  x := !x + 1;
  printf "%d\n" !x;

  (* For loop *)
  let permute array =
    let length = Array.length array in
    for i = 0 to length - 2 do
      let j = i + Random.int (length - i) in
      let tmp = array.(i) in
      array.(i) <- array.(j);
      array.(j) <- tmp
    done
  in
  let ar = Array.init 20 ~f:(fun i -> i) in
  permute ar;

  (* While loop *)
  let find_first_negative_entry array =
    let pos = ref 0 in
    while !pos < Array.length array && array.(!pos) >= 0 do
      pos := !pos + 1
    done;
    if !pos = Array.length array then None else Some !pos
  in
  let _ = find_first_negative_entry [| 1; 2; 0; -1; 3 |] in

  ()
