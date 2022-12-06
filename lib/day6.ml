open! Base
open Stdio
open! Str

let inspect x = ExtLib.print x; x

let make_range s e  =
  let rec range_fun s e lst =
    if s = e then lst
    else s :: range_fun (s+1) e lst
  in range_fun s e []

let day_6 = 0


let%test "day 6 - demo test" =
  let result = day_6
  in
  printf "Result day_6 (demo): %s\n" (ExtLib.dump result);
  result = 0

(* let prep_input = *)
(*   In_channel.read_all "/Users/mbergmann/Development/MySources/aoc2022/input/day5_1.txt" *)

