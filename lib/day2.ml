open! Base
open Stdio

let inspect x = ExtLib.print x; x

type shape_type = Rock | Paper | Scissor
type shape = Shape of shape_type * int

type round = shape * shape

type win_type = Win | Loose | Draw
type play_result = Result of win_type * shape

let rock = Shape(Rock, 1)
let paper = Shape(Paper, 2)
let scissor = Shape(Scissor, 3)

exception IncompleteRound
exception UnknownShape

let shape_of_str str = match str with
  | "A" | "X" -> rock
  | "B" | "Y" -> paper
  | "C" | "Z" -> scissor
  | _ -> raise UnknownShape

let to_shapes_1 fst snd = ((shape_of_str fst), (shape_of_str snd))

let to_shapes_2 fst snd =
  match (snd, (shape_of_str fst)) with
  | ("X", Shape(Rock, _)) -> (rock, scissor)
  | ("X", Shape(Paper, _)) -> (paper, rock)
  | ("X", Shape(Scissor, _)) -> (scissor, paper)
  | ("Y", Shape(Rock, _)) -> (rock, rock)
  | ("Y", Shape(Paper, _)) -> (paper, paper)
  | ("Y", Shape(Scissor, _)) -> (scissor, scissor)
  | ("Z", Shape(Rock, _)) -> (rock, paper)
  | ("Z", Shape(Paper, _)) -> (paper, scissor)
  | ("Z", Shape(Scissor, _)) -> (scissor, rock)
  | _ -> raise UnknownShape

let day_2 input (to_shapes_fun : string -> string -> round) =
  let lines = String.split_lines input in
  let rounds = List.map lines ~f:(fun line ->
                   match (String.split line ~on:' ') with
                   | [] -> raise IncompleteRound
                   | _ :: [] -> raise IncompleteRound
                   | fst :: snd :: _ -> (to_shapes_fun fst snd)) in
  let results = List.map rounds ~f:(fun round ->
                    match round with
                    | (Shape(st, _), Shape(stme, _)) -> match (st, stme) with
                                  | (Rock, Rock) -> Result(Draw, rock)
                                  | (Paper, Paper) -> Result(Draw, paper)
                                  | (Scissor, Scissor) -> Result(Draw, scissor)
                                  | (Rock, Scissor) -> Result(Loose, scissor)
                                  | (Paper, Rock) -> Result(Loose, rock)
                                  | (Scissor, Paper) -> Result(Loose, paper)
                                  | (Scissor, Rock) -> Result(Win, rock)
                                  | (Rock, Paper) -> Result(Win, paper)
                                  | (Paper, Scissor) -> Result(Win, scissor)) in
  let scores = List.map results ~f:(fun result ->
                   match result with
                   | Result(Win, Shape(_, v)) -> (6 + v)
                   | Result(Loose, Shape(_, v)) -> (0 + v)
                   | Result(Draw, Shape(_, v)) -> (3 + v)) in
  let score = List.fold scores ~init:0 ~f:(+) in
  score

let demo_input = "A Y
B X
C Z"

let%test "day 2 - demo test" =
  let result = day_2 demo_input to_shapes_1
  in
  printf "Result day_2 (demo): %s\n" (ExtLib.dump result);
  result = 15

let prep_input =
  In_channel.read_all "/Users/mbergmann/Development/MySources/aoc2022/input/day2_1.txt"

let%test "day 2 - real test" =
  let result = day_2 prep_input to_shapes_1
  in
  printf "Result day_2 (real): %s\n" (ExtLib.dump result);
  result = 13565

let%test "day 2-2 - demo test" =
  let result = day_2 demo_input to_shapes_2
  in
  printf "Result day_2-2 (demo): %s\n" (ExtLib.dump result);
  result = 12

let%test "day 2-2 - real test" =
  let result = day_2 prep_input to_shapes_2
  in
  printf "Result day_2-2 (real): %s\n" (ExtLib.dump result);
  true
