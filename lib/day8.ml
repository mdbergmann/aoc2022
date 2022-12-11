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
  let rows = List.map lines ~f:(fun line ->
                 List.map (String.to_list line) ~f:(fun c ->
                     (Char.to_int c) - (Char.to_int '0'))) in
  let visible_trees_from_left_on_row row =
    let visible_index = ref 0 in
    let visible_elem = ref 0 in
    List.iteri row ~f:(fun i elem ->
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
    List.iteri (List.rev row) ~f:(fun i elem ->
        if elem > !visible_elem
        then (
          visible_elem := elem;
          visible_index := i;
      ));
    let adjusted_index =
      (List.length row) - (!visible_index + 1) in
    (!visible_elem, adjusted_index)
  in

  let visible_trees_count_hori_inner row =
    let (_, from_left_index) = visible_trees_from_left_on_row row in
    let (_, from_right_index) = visible_trees_from_right_on_row row in

    if (from_left_index > 0) && (from_right_index < (List.length row))
    then 
      if (from_left_index = from_right_index)
      then 1
      else 2
    else 0 in

  let effective_rows = (List.drop_last_exn (List.drop rows 1)) in
  let visible_trees_rows = (List.fold (List.map effective_rows
                                         ~f:visible_trees_count_hori_inner)
                             ~init:0
                             ~f:(+)) in
  
  ExtLib.print visible_trees_rows;
  assert (visible_trees_rows = 3);

  let cols =
    let outer = Array.create ~len:(List.length rows) [||] in
    List.iteri rows ~f:(fun _i row ->
        List.iteri row
          ~f:(fun j elem ->
            let inner = (Array.get outer j) in
            let new_inner = Array.append inner [|elem|] in
            Array.set outer j new_inner;
          );
      );
    Array.to_list (Array.map outer ~f:Array.to_list) in  
  ExtLib.print cols;

  let effective_cols = (List.drop_last_exn (List.drop cols 1)) in
  let visible_trees_cols = (List.fold (List.map effective_cols
                                         ~f:visible_trees_count_hori_inner)
                             ~init:0
                             ~f:(+)) in  
  ExtLib.print visible_trees_cols;
  assert (visible_trees_cols = 5);
  
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
