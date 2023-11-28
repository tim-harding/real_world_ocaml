open Base
open Stdio

(* Width subtyping *)
type shape = < area : float >
type square = < area : float ; width : int >

let square w =
  object
    method area = Float.of_int (w * w)
    method width = w
  end

let print_area (s : shape) = printf "%f\n" s#area

(* Depth subtyping *)
type circle = < area : float ; radius : int >
type item = < shape : shape >

let circle r =
  object
    method area =
      let pi_r = 3.14 *. Float.of_int r in
      pi_r *. pi_r

    method radius = r
  end

let stack init =
  object
    val mutable v = init

    method pop =
      match v with
      | hd :: tl ->
          v <- tl;
          Some hd
      | [] -> None

    method push hd = v <- hd :: v
  end

let array_stack init =
  object
    val s = Stack.of_list init
    method pop = Stack.pop s
    method push hd = Stack.push s hd
  end

let run () =
  let test_stack s =
    s#push 4;
    let print_pop s =
      match s#pop with
      | None -> printf "None\n"
      | Some x -> printf "Some(%d)\n" x
    in
    print_pop s;
    print_pop s;
    print_pop s;
    print_pop s
  in

  test_stack (stack [ 0; 2 ]);
  test_stack (array_stack [ 0; 2 ]);

  (* Must explicitly convert with :> *)
  print_area (square 10 :> shape);

  let coin =
    object
      method shape = circle 5
      method color = "silver"
    end
  in

  let book =
    object
      method shape = square 10
    end
  in

  let _items = [ (coin :> item); (book :> item) ] in

  ()
