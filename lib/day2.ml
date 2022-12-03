open! Base
open Stdio

let inspect x = ExtLib.print x; x

type shape_type = Rock | Paper | Scissor
type shape = {t : shape_type; value : int}

type win_type = Win | Loose | Draw
type play_result = {t : win_type; play_shape : shape}

let rock = {t = Rock; value = 1}
let paper = {t = Paper; value = 2}
let scissor = {t = Scissor; value = 3}

exception IncompleteRound
exception UnknownShape

let shape_of_str str = match str with
  | "A" | "X" -> rock
  | "B" | "Y" -> paper
  | "C" | "Z" -> scissor
  | _ -> raise UnknownShape

let day_2 input =
  let lines = String.split_lines input in
  let rounds = List.map lines ~f:(fun line ->
                   match (String.split line ~on:' ') with
                   | [] -> raise IncompleteRound
                   | _ :: [] -> raise IncompleteRound
                   | fst :: snd :: _ -> ((shape_of_str fst,
                                          shape_of_str snd))) in
  let results = List.map rounds ~f:(fun round ->
                    match round with
                    | ({t = Rock; _}, {t = Rock; _}) -> {t = Draw; play_shape = rock}
                    | ({t = Paper; _}, {t = Paper; _}) -> {t = Draw; play_shape = paper}
                    | ({t = Scissor; _}, {t = Scissor; _}) -> {t = Draw; play_shape = scissor}
                    | ({t = Rock; _}, {t = Scissor; _}) -> {t = Loose; play_shape = scissor}
                    | ({t = Paper; _}, {t = Rock; _}) -> {t = Loose; play_shape = rock}
                    | ({t = Scissor; _}, {t = Paper; _}) -> {t = Loose; play_shape = paper}
                    | ({t = Scissor; _}, {t = Rock; _}) -> {t = Win; play_shape = rock}
                    | ({t = Rock; _}, {t = Paper; _}) -> {t = Win; play_shape = paper}
                    | ({t = Paper; _}, {t = Scissor; _}) -> {t = Win; play_shape = scissor}) in
  let scores = List.map results ~f:(fun result ->
                   match result with
                   | {t = Win; _} -> (6 + result.play_shape.value)
                   | {t = Loose; _} -> (0 + result.play_shape.value)
                   | {t = Draw; _} -> (3 + result.play_shape.value)) in
  let score = List.fold scores ~init:0 ~f:(+) in
  score

let demo_input = "A Y
B X
C Z"

let%test "day 2 - demo test" =
  let result = day_2 demo_input
  in
  printf "Result day_2: %s\n" (ExtLib.dump result);
  result = 15

(* let prep_input = *)
(*   In_channel.read_all "/Users/mbergmann/Development/MySources/aoc2022/input/day1_1.txt" *)
