open! Base
open Stdio

let inspect x = ExtLib.print x; x

let day_2 = 0

let%test "day 2 - demo test" =
  let result = day_2
  in
  printf "Result 2: %s\n" (ExtLib.dump result);
  result = 0

(* let prep_input = *)
(*   In_channel.read_all "/Users/mbergmann/Development/MySources/aoc2022/input/day1_1.txt" *)
