open! Base
open Stdio

let inspect x = ExtLib.print x; x

let make_range s e  =
  let rec range_fun =
    fun s e lst -> if s = e then lst
                   else s :: range_fun (s+1) e lst
  in range_fun s e []

exception Error

let day_4 input =
  let str_pairs = List.map (String.split_lines input) ~f:(fun line ->
                      String.split line ~on:',') in
  ExtLib.print str_pairs;
  let num_pairs = List.map str_pairs ~f:(fun str_pair ->
                      List.map str_pair ~f:(fun elem ->
                          match String.split elem ~on:'-' with
                          | side1 :: side2 :: _ ->
                             make_range (Int.of_string side1) ((Int.of_string side2) + 1)
                          | _ -> raise Error
                        )
                    ) in
  ExtLib.print num_pairs;
  let containing = List.filter num_pairs ~f:(fun pair ->
                       match pair with
                       | side1 :: side2 :: _ ->
                          Caml.(||)
                            (List.for_all side1 ~f:(fun elem1 ->
                                 List.exists side2 ~f:(fun elem2 -> elem1 = elem2)))
                            (List.for_all side2 ~f:(fun elem1 ->
                                 List.exists side1 ~f:(fun elem2 -> elem1 = elem2)))
                       | _ -> raise Error) in
  ExtLib.print containing;
  let sum = List.length containing in
  sum

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
