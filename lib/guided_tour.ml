open Stdio

let hello () =
  let _ = printf "%d\n" (3_000 + 4) in
  let _ = printf "%F\n" (3.5 +. 6.) in
  ()
