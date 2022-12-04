open! Base
open Stdio

let inspect x = ExtLib.print x; x

let day_4 _ = 0

let%test "day 4 - demo test" =
  let result = day_4 0
  in
  printf "Result day_4 (demo): %s\n" (ExtLib.dump result);
  result = 0

(* let prep_input = *)
(*   In_channel.read_all "/Users/mbergmann/Development/MySources/aoc2022/input/day3_1.txt" *)
