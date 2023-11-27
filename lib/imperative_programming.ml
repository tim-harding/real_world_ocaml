open Base
open Stdio

(* TODO: Doubly linked list example *)

module String_pair = struct
  type t = string * string [@@deriving sexp_of, hash, compare]
end

let _speak () =
  Out_channel.output_string stdout "Your name: ";
  Out_channel.flush stdout;
  match In_channel.(input_line stdin) with
  | None -> failwith "No name provided"
  | Some name ->
      Out_channel.output_string stdout
        (String.concat [ "Your name is\n  "; name; ".\n" ]);
      Out_channel.flush stdout

let run () =
  let array = [| 1; 2; 3 |] in
  array.(2) <- 4;

  (* String as bytes *)
  let b = Bytes.of_string "foobar" in
  Bytes.set b 0 (Char.uppercase (Bytes.get b 0));
  print_endline (Bytes.to_string b);

  (* Ref cells, single mutable values *)
  let x = ref 1 in
  printf "%d\n" !x;
  x := !x + 1;
  printf "%d\n" !x;

  (* For loops *)
  for i = 0 to 3 do
    printf "i = %d\n" i
  done;

  (* While loops *)
  let rev_inplace ar =
    let i = ref 0 in
    let j = ref (Array.length ar - 1) in
    while !i < !j do
      let tmp = ar.(!i) in
      ar.(!i) <- ar.(!j);
      ar.(!j) <- tmp;
      Int.incr i;
      Int.decr j
    done
  in

  let ar = [| 4; 6; 8 |] in
  rev_inplace ar;
  (* %! flushes the output *)
  Array.iter ar ~f:(fun x -> printf "%d, %!" x);
  print_endline "";

  (* Laziness *)
  let v =
    lazy
      (print_endline "performing lazy computation";
       Float.sqrt 16.)
  in
  printf "%f\n" (Lazy.force v);
  printf "%f\n" (Lazy.force v);

  (* Memoization *)
  let memoize m f =
    let memo_table = Hashtbl.create m in
    fun x -> Hashtbl.find_or_add memo_table x ~default:(fun () -> f x)
  in
  let _memo_rec m f_norec x =
    (* This reference avoids rec, which wouldn't work here.
       lec rec only allows function definitions, constructors, or lazy,
       which excludes what we'd be doing here. *)
    let fref = ref (fun _ -> assert false) in
    let f = memoize m (fun x -> f_norec !fref x) in
    fref := f;
    f x
  in
  let memo_rec m f_norec x =
    (* Can also be defined using lazy *)
    let rec f = lazy (memoize m (fun x -> f_norec (force f) x)) in
    (force f) x
  in
  let fib_norec fib i = if i <= 1 then i else fib (i - 1) + fib (i - 2) in
  let fib = memo_rec (module Int) fib_norec in
  printf "%d\n" (fib 20);

  let edit_distance =
    memo_rec
      (module String_pair)
      (fun edit_distance (s, t) ->
        match (String.length s, String.length t) with
        | 0, x | x, 0 -> x
        | len_s, len_t ->
            let s' = String.drop_suffix s 1 in
            let t' = String.drop_suffix t 1 in
            let cost_to_drop_both =
              if Char.( = ) s.[len_s - 1] t.[len_t - 1] then 0 else 1
            in
            List.reduce_exn ~f:Int.min
              [
                edit_distance (s', t) + 1;
                edit_distance (s, t') + 1;
                edit_distance (s', t') + cost_to_drop_both;
              ])
  in
  printf "%d\n" (edit_distance ("OCaml", "ocaml"));

  (* Alternative printf *)
  eprintf "An error\n";
  print_endline (Printf.sprintf "I'm #%d!" 1);

  (* File IO *)
  let create_number_file filename numbers =
    let outc = Out_channel.create filename in
    List.iter numbers ~f:(fun x -> Out_channel.fprintf outc "%d\n" x);
    Out_channel.close outc
  in

  let _sum_file filename =
    let file = In_channel.create filename in
    Exn.protect
      ~finally:(fun () -> In_channel.close file)
      ~f:(fun () ->
        let numbers = List.map ~f:Int.of_string (In_channel.input_lines file) in
        List.fold ~init:0 ~f:( + ) numbers)
  in

  (* More succinctly, *)
  let _sum_file filename =
    In_channel.with_file filename ~f:(fun file ->
        let numbers = List.map ~f:Int.of_string (In_channel.input_lines file) in
        List.fold ~init:0 ~f:( + ) numbers)
  in

  (* And without reading the whole file into memory, *)
  let sum_file filename =
    In_channel.with_file filename ~f:(fun file ->
        In_channel.fold_lines file ~init:0 ~f:(fun sum line ->
            sum + Int.of_string line))
  in

  create_number_file "numbers.txt" [ 1; 2; 3; 4; 5 ];
  printf "%d\n" (sum_file "numbers.txt");

  (* Weak polymorphism *)
  let remember =
    let cache = ref None in
    fun x ->
      match !cache with
      | Some y -> y
      | None ->
          cache := Some x;
          x
  in

  let _ = remember 3 in

  (* Not allowed because remember is weakly polymorphic,
       meaning that the type of x is determined by the
     type of the first thing it is called with *)
  (* let _ = remember "hi" in *)

  (* Partial application and the value restriction *)
  (*
    Inference is only allowed for simple values:
      - Constants
      - Constructors that only contain other simple values
      - Function declarations
      - let bindings of the form let var = expr1 in expr2, where both expr1 and expr2 are simple values
    A function with mutable state that persists across calls, 
    like memoize, can only be weakly polymorphic.
    Polymorphic application can trigger this:
  *)
  let _list_init_10 = List.init 10 in
  (* The above isn't a simple value so it is weakly polymorphic.
     We can get around this with eta expansion: *)
  let _list_init_10 ~f = List.init 10 ~f in

  (* TODO: Relaxing the value restriction section *)
  ()
