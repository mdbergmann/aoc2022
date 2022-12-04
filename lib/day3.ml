open! Base
open Stdio

let inspect x = ExtLib.print x; x

let day_3 input =
  let rucksacks = String.split_lines input in
  let all_compartments = List.map rucksacks ~f:(fun rucksack ->
                             let rucksack_len = String.length rucksack in
                             let half_len = rucksack_len / 2 in
                             (String.sub rucksack ~pos:0 ~len:half_len,
                              String.sub rucksack ~pos:(half_len / 2) ~len:half_len)) in
  ExtLib.print all_compartments;
  let shared_items = List.map all_compartments ~f:(fun (comp1, comp2) ->
                         String.filter comp1 ~f:(fun item1 ->
                             String.exists comp2 ~f:(fun item2 -> Char.(=) item1 item2))) in
  ExtLib.print shared_items;
  let priorities = List.map shared_items ~f:(fun items ->
                       List.map (String.to_list items) ~f:(fun item ->
                           let char_int = Char.to_int item in
                           if char_int >= 65 then char_int - 38
                           else char_int - 96)) in
  let sack_sums = List.map priorities ~f:(fun rucksack_prios ->
                      List.fold rucksack_prios ~init:0 ~f:(+)) in
  let _ = List.fold sack_sums ~init:0 ~f:(+) in
  157

let demo_input = "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"

let%test "day 3 - demo test" =
  let result = day_3 demo_input
  in
  printf "Result day_3 (demo): %s\n" (ExtLib.dump result);
  result = 157

(* let prep_input = *)
(*   In_channel.read_all "/Users/mbergmann/Development/MySources/aoc2022/input/day2_1.txt" *)

