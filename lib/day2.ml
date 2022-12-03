open! Base
open Stdio

type shape = Rock of int | Paper of int | Scissors of int

let inspect x = ExtLib.print x; x

let demo_input = "A Y
B X
C Z"

let day_2 input =
  let _ = String.split_lines input in
  15

let%test "day 2 - demo test" =
  let result = day_2 demo_input
  in
  printf "Result day_2: %s\n" (ExtLib.dump result);
  result = 15

(* let prep_input = *)
(*   In_channel.read_all "/Users/mbergmann/Development/MySources/aoc2022/input/day1_1.txt" *)
