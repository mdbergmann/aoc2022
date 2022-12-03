open! Base
open Stdio

type shape = Shape of int

let rock = Shape(1)
let paper = Shape(2)
let scissor = Shape(3)

let inspect x = ExtLib.print x; x

let demo_input = "A Y
B X
C Z"

exception IncompleteRound
exception UnknownShape

let shape_of_str str = match str with
  | "A" | "X" -> rock
  | "B" | "Y" -> paper
  | "C" | "Z" -> scissor
  | _ -> raise UnknownShape

let day_2 input =
  let lines = String.split_lines input in
  let rounds = List.map lines ~f:(fun line -> match (String.split line ~on:' ') with
                                              | [] -> raise IncompleteRound
                                              | _ :: [] -> raise IncompleteRound
                                              | fst :: snd :: _ -> ((shape_of_str fst,
                                                                     shape_of_str snd))) in
  let _ = List.map rounds ~f:(fun round -> match round with
                                           | (_, _) -> ()) in
  15

let%test "day 2 - demo test" =
  let result = day_2 demo_input
  in
  printf "Result day_2: %s\n" (ExtLib.dump result);
  result = 15

(* let prep_input = *)
(*   In_channel.read_all "/Users/mbergmann/Development/MySources/aoc2022/input/day1_1.txt" *)
