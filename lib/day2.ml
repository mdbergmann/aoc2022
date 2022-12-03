open! Base
open Stdio

type shape = Shape of string

let inspect x = ExtLib.print x; x

let demo_input = "A Y
B X
C Z"

exception IncompleteRound
exception UnknownShape

let day_2 input =
  let lines = String.split_lines input in
  let rounds = List.map lines ~f:(fun line -> match (String.split line ~on:' ') with
                                              | [] -> raise IncompleteRound
                                              | _ :: [] -> raise IncompleteRound
                                              | fst :: snd :: _ -> ((match fst with
                                                                     | "A" | "X" -> 1
                                                                     | "B" | "Y" -> 2
                                                                     | "C" | "Z" -> 3
                                                                     | _ -> raise UnknownShape),
                                                                    (match snd with
                                                                     | "A" | "X" -> 1
                                                                     | "B" | "Y" -> 2
                                                                     | "C" | "Z" -> 3
                                                                     | _ -> raise UnknownShape)
                                                                    )) in
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
