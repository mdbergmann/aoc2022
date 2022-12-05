open! Base
open Stdio

let inspect x = ExtLib.print x; x

let make_range s e  =
  let rec range_fun s e lst =
    if s = e then lst
    else s :: range_fun (s+1) e lst
  in range_fun s e []

let day_5 input =
  let lines = String.split_lines input in
  let (creates_numbers_line_index, creates_numbers_line) = match
      List.findi lines ~f:(fun _ line -> not (String.contains line '[')) with
    | Some (l_i, line) -> (l_i, line)
    | None -> assert false in
  ExtLib.print (creates_numbers_line_index, creates_numbers_line);
  "CMZ"

let demo_input = "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"

let%test "day 5 - demo test" =
  let result = day_5 demo_input
  in
  printf "Result day_5 (demo): %s\n" (ExtLib.dump result);
  String.equal result "CMZ"

(* let prep_input = *)
(*   In_channel.read_all "/Users/mbergmann/Development/MySources/aoc2022/input/day5_1.txt" *)
