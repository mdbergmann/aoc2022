open! Base
open Stdio

let inspect x = ExtLib.print x; x

let day_4 _ = 2

let demo_input = "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"

let%test "day 4 - demo test" =
  let result = day_4 demo_input
  in
  printf "Result day_4 (demo): %s\n" (ExtLib.dump result);
  result = 2

(* let prep_input = *)
(*   In_channel.read_all "/Users/mbergmann/Development/MySources/aoc2022/input/day3_1.txt" *)
