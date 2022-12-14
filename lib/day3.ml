open! Base
open Stdio

let inspect x = ExtLib.print x; x

exception Error

let common_day_3 shared_items =
  let priorities = List.map shared_items ~f:(fun items ->
                       List.map (String.to_list items) ~f:(fun item ->
                           let char_int = Char.to_int item in
                           if char_int >= 97 then char_int - 96
                           else char_int - 38)) in
  let filtered_dups = List.map priorities ~f:(fun prios ->
                          List.dedup_and_sort prios ~compare:(fun _ _ -> 0)) in
  let sack_sums = List.map filtered_dups ~f:(fun rucksack_prios ->
                      List.fold rucksack_prios ~init:0 ~f:(+)) in
  let sum = List.fold sack_sums ~init:0 ~f:(+) in
  sum

let day_3_1 input =
  let rucksacks = String.split_lines input in
  let all_compartments = List.map rucksacks ~f:(fun rucksack ->
                             let rucksack_len = String.length rucksack in
                             let half_len = rucksack_len / 2 in
                             (String.sub rucksack ~pos:0 ~len:half_len,
                              String.sub rucksack ~pos:half_len ~len:half_len)) in
  let shared_items = List.map all_compartments ~f:(fun (comp1, comp2) ->
                         String.filter comp1 ~f:(fun item1 ->
                             String.exists comp2 ~f:(fun item2 -> Char.(=) item1 item2))) in
  common_day_3 shared_items

let day_3_2 input =
  let rucksacks = String.split_lines input in
  let rucksack_groups = List.groupi rucksacks ~break:(fun i _ _ -> (Int.rem i 3) = 0) in
  let shared_items = List.map rucksack_groups ~f:(fun group ->
                         match group with
                         | sack1 :: sack2 :: sack3 :: _ ->
                            String.filter sack1 ~f:(fun item1 ->
                                (String.exists sack2 ~f:(fun item2 -> Char.(=) item1 item2)) &&
                                  (String.exists sack3 ~f:(fun item3 -> Char.(=) item1 item3)))
                         | _ -> raise Error
                       ) in
  common_day_3 shared_items


let demo_input = "vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw"

let%test "day 3 - demo test" =
  let result = day_3_1 demo_input
  in
  printf "Result day_3 (demo): %s\n" (ExtLib.dump result);
  result = 157

let prep_input =
  In_channel.read_all "/Users/mbergmann/Development/MySources/aoc2022/input/day3_1.txt"

let%test "day 3 - real test" =
  let result = day_3_1 prep_input
  in
  printf "Result day_3 (real): %s\n" (ExtLib.dump result);
  result = 7568

let%test "day 3-2 - demo test" =
  let result = day_3_2 demo_input
  in
  printf "Result day_3-2 (demo): %s\n" (ExtLib.dump result);
  result = 70

let%test "day 3-2 - real test" =
  let result = day_3_2 prep_input
  in
  printf "Result day_3-2 (real): %s\n" (ExtLib.dump result);
  result = 2780
