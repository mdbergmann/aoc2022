open! Base
open Stdio

let inspect x = ExtLib.print x; x

let day_3 = 157

let demo_input = "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"

let%test "day 3 - demo test" =
  let result = day_3
  in
  printf "Result day_3 (demo): %s\n" (ExtLib.dump result);
  result = 157

(* let prep_input = *)
(*   In_channel.read_all "/Users/mbergmann/Development/MySources/aoc2022/input/day2_1.txt" *)

