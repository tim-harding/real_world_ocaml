open Base
open Stdio

(* Parametric record types *)
type 'a with_line_number = { item : 'a; line_number : int }

(* (string -> 'a) -> string -> 'a with_line_number list *)
let parse_lines parse file_contents =
  let lines = String.split ~on:'\n' file_contents in
  List.mapi lines ~f:(fun line_number line ->
      { item = parse line; line_number = line_number + 1 })

(* Destructuring parameter *)
let print_numbered_line { item; line_number = n } = printf "%d: %s\n" n item

(* Modules help disambiguate types with the same fields *)
module Patient = struct
  type t = { id : string; name : string } [@@deriving fields]
end

module Doctor = struct
  type t = { mutable id : string; name : string }
end

let _make_patient id name = { Patient.id; name }

(* Can also qualify a field by a module name... *)
let _patient_id t = t.Patient.id

(* ...or, with the fields derive, like this *)
let _patient_id t = Patient.id t

(* Partial update *)
let _update_patient_id (patient : Patient.t) id = { patient with id }
let _update_doctor_id (doctor : Doctor.t) id = doctor.id <- id

let run () =
  parse_lines (fun s -> s) "Things\nand\nstuff"
  |> List.iter ~f:print_numbered_line
