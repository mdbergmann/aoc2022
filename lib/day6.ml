open! Base
open Stdio

let inspect x = ExtLib.print x; x

let make_range s e  =
  let rec range_fun s e lst =
    if s = e then lst
    else s :: range_fun (s+1) e lst
  in range_fun s e []

let day_6 input =
  let acc = String.fold input ~init:"" ~f:(fun acc c ->
      if (String.length acc) < 4 then acc ^ String.of_char c
      else acc
    ) in
  ExtLib.print acc;
  7

let%test "day 6 - demo test, 1" =
  let result = day_6 "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
  in
  printf "Result day_6 (demo, 1): %s\n" (ExtLib.dump result);
  result = 7

(* let prep_input = *)
(*   In_channel.read_all "/Users/mbergmann/Development/MySources/aoc2022/input/day5_1.txt" *)

