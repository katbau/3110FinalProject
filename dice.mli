type dice = int

(** generates a random int from numbers 1-6**)
val roll_die: unit -> dice


(** compares two different die values and returns -1 if the
 ** first die is smaller, 0 if they are equal and 1 if the
 ** second one is bigger **)
val compare: dice -> dice -> int

