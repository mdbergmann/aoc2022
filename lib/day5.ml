open! Base
open Stdio

let inspect x = ExtLib.print x; x

let make_range s e  =
  let rec range_fun =
    fun s e lst -> if s = e then lst
                   else s :: range_fun (s+1) e lst
  in range_fun s e []

let day_5 = 0


let demo_input = "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"

let%test "day 5 - demo test" =
  let result = day_5
  in
  printf "Result day_5 (demo): %s\n" (ExtLib.dump result);
  result = 0

(* let prep_input = *)
(*   In_channel.read_all "/Users/mbergmann/Development/MySources/aoc2022/input/day5_1.txt" *)
