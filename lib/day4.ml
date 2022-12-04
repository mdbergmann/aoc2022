open! Base
open Stdio

let inspect x = ExtLib.print x; x

let make_range s e  =
  let rec range_fun =
    fun s e lst -> if s = e then lst
                   else s :: range_fun (s+1) e lst
  in range_fun s e []

exception Error

let containing input =
  List.filter input ~f:(fun (left, right) ->
      Caml.(||)
        (List.for_all left ~f:(fun elem1 ->
             List.exists right ~f:(fun elem2 -> elem1 = elem2)))
        (List.for_all right ~f:(fun elem1 ->
             List.exists left ~f:(fun elem2 -> elem1 = elem2))))

let overlapping input =
  List.filter input ~f:(fun (left, right) ->
      List.exists left ~f:(fun elem1 ->
          List.exists right ~f:(fun elem2 -> elem1 = elem2)))

let day_4 input filter_fun =
  let str_pairs = List.map (String.split_lines input) ~f:(fun line ->
                      String.split line ~on:',') in
  let num_pairs = List.map str_pairs ~f:(fun str_pair ->
                      let num_pair =
                        (List.map str_pair ~f:(fun elem ->
                             match String.split elem ~on:'-' with
                             | left :: right :: _ ->
                                make_range (Int.of_string left) ((Int.of_string right) + 1)
                             | _ -> raise Error
                        )) in
                      let to_tuple = 
                        match num_pair with
                        | left :: right :: _ -> (left, right)
                        | _ -> raise Error in
                      to_tuple) in
  let filtered = filter_fun num_pairs in
  let sum = List.length filtered in
  sum

let demo_input = "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8"

let%test "day 4 - demo test" =
  let result = day_4 demo_input containing
  in
  printf "Result day_4 (demo): %s\n" (ExtLib.dump result);
  result = 2

let%test "day 4_2 - demo test" =
  let result = day_4 demo_input overlapping
  in
  printf "Result day_4_2 (demo): %s\n" (ExtLib.dump result);
  result = 4

let prep_input =
  In_channel.read_all "/Users/mbergmann/Development/MySources/aoc2022/input/day4_1.txt"

let%test "day 4 - real test" =
  let result = day_4 prep_input containing
  in
  printf "Result day_4 (real): %s\n" (ExtLib.dump result);
  result = 532

let%test "day 4_2 - real test" =
  let result = day_4 prep_input overlapping
  in
  printf "Result day_4_2 (real): %s\n" (ExtLib.dump result);
  result = 854
