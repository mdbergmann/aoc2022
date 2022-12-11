open Base
open Stdio

let inspect x = ExtLib.print x; x

let make_range s e  =
  let rec range_fun s e lst =
    if s = e then lst
    else s :: range_fun (s+1) e lst
  in range_fun s e []

let day_8 input =
  let lines = String.split_lines input in
  let rows = List.to_array (List.map lines ~f:(fun line ->
                                Array.map (String.to_array line) ~f:(fun c ->
                                    (Char.to_int c) - (Char.to_int '0')))) in
  let visible_trees_from_left_on_row row =
    let visible_index = ref 0 in
    let visible_elem = ref 0 in
    Array.iteri row ~f:(fun i elem ->
        if elem > !visible_elem
        then (
          visible_elem := elem;
          visible_index := i;
      ));
    (!visible_elem, !visible_index)
  in

  let visible_trees_from_right_on_row row =
    let visible_index = ref 0 in
    let visible_elem = ref 0 in
    Array.iteri (Array.rev row) ~f:(fun i elem ->
        if elem > !visible_elem
        then (
          visible_elem := elem;
          visible_index := i;
      ));
    let adjusted_index =
      (Array.length row) - (!visible_index + 1) in
    (!visible_elem, adjusted_index)
  in
  
  let visible_hori_row_1_left = visible_trees_from_left_on_row (Array.get rows 0) in
  let visible_hori_row_1_right = visible_trees_from_right_on_row (Array.get rows 0) in
  ExtLib.print visible_hori_row_1_left;
  ExtLib.print visible_hori_row_1_right;
  21

let demo_input = "30373
25512
65332
33549
35390"

let%test "day 8 - demo test, 1" =
  let result = day_8 demo_input
  in
  printf "Result day_8 (demo, 1): %s\n" (ExtLib.dump result);
  result = 21

(* let prep_input = *)
(*   In_channel.read_all "/Users/mbergmann/Development/MySources/aoc2022/input/day6_1.txt" *)
