type dice = int

let roll_die () =
  (Random.int 6) + 1

let compare =
  Pervasives.compare