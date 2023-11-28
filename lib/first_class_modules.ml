open Base

module type X_int = sig
  val x : int
end

module Three : X_int = struct
  let x = 3
end

module Four : X_int = struct
  let x = 4
end

(* Create the first-class module *)
let three = (module Three : X_int)
let numbers = [ three; (module Four) ]

(* Convert back to a regular module *)
module New_three = (val three : X_int)

let _to_int m =
  let module M = (val m : X_int) in
  M.x

let to_int (module M : X_int) = M.x

let plus m1 m2 =
  (module struct
    let x = to_int m1 + to_int m2
  end : X_int)

let six = plus three three

(* A common interface *)
module type Bumpable = sig
  type t

  val bump : t -> t
end

module Int_bumper = struct
  type t = int

  let bump n = n + 1
end

module Float_bumper = struct
  type t = float

  let bump n = n +. 1.
end

(* Locally abstract types *)
module type Comparable = sig
  type t

  val compare : t -> t -> int
end

let create_comparable (type a) compare =
  (module struct
    type t = a

    let compare = compare
  end : Comparable
    with type t = a)

let run () = ()

(* TODO: Work through example *)
