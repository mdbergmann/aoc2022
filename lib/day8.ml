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
  let visible_trees_from_left_on_row row index =
    let visible_count = ref 0 in
    let recorded_indices = ref [] in
    let visible_elem = ref (List.nth_exn row 0) in
    List.iteri (List.drop_last_exn row) ~f:(fun i elem ->
        if elem > !visible_elem
        then (
          visible_elem := elem;
          visible_count := !visible_count + 1;
          recorded_indices := (index, i) :: !recorded_indices;
      ));
    (!recorded_indices, !visible_count)
  in

  let visible_trees_from_right_on_row row index =
    let visible_count = ref 0 in
    let recorded_indices = ref [] in
    let visible_elem = ref (List.last_exn row) in
    List.iteri (List.rev (List.drop row 1)) ~f:(fun i elem ->
        if elem > !visible_elem
        then (
          visible_elem := elem;
          visible_count := !visible_count + 1;
          let adjusted_index = (List.length row) - (i + 1) in
          recorded_indices := (index, adjusted_index) :: !recorded_indices;
      ));
    (!recorded_indices, !visible_count)
  in

  let visible_trees_count_hori_inner row index =
    let (l_indices, _from_left_count) = visible_trees_from_left_on_row row index in
    let (r_indices, _from_right_count) = visible_trees_from_right_on_row row index in

    let new_r_indices = List.filter r_indices
                          ~f:(fun (row, col) ->
                            not (List.exists l_indices
                              ~f:(fun (rowl, coll) ->
                                row = rowl && col = coll
                              ))
                          ) in
    ExtLib.print new_r_indices;
    let indices = List.append l_indices new_r_indices in
    ExtLib.print indices;
    let count = List.length indices in
    count
  in

  assert ((visible_trees_count_hori_inner (List.nth_exn rows 0) 0) = 1);
  assert ((visible_trees_count_hori_inner (List.nth_exn rows 1) 1) = 2);
  assert ((visible_trees_count_hori_inner (List.nth_exn rows 2) 2) = 2);
  assert ((visible_trees_count_hori_inner (List.nth_exn rows 3) 3) = 1);
  assert ((visible_trees_count_hori_inner (List.nth_exn rows 4) 4) = 2);
  
  (* let effective_rows = (List.drop_last_exn (List.drop rows 1)) in *)
  (* let visible_trees_rows = (List.fold (List.map effective_rows *)
  (*                                        ~f:visible_trees_count_hori_inner) *)
  (*                            ~init:0 *)
  (*                            ~f:(+)) in *)
  
  (* ExtLib.print visible_trees_rows; *)
  (* assert (visible_trees_rows = 5); *)

  (* let cols = *)
  (*   let outer = Array.create ~len:(List.length rows) [||] in *)
  (*   List.iteri rows ~f:(fun _i row -> *)
  (*       List.iteri row *)
  (*         ~f:(fun j elem -> *)
  (*           let inner = (Array.get outer j) in *)
  (*           let new_inner = Array.append inner [|elem|] in *)
  (*           Array.set outer j new_inner; *)
  (*         ); *)
  (*     ); *)
  (*   Array.to_list (Array.map outer ~f:Array.to_list) in *)
  (* ExtLib.print cols; *)

  (* let effective_cols = (List.drop_last_exn (List.drop cols 1)) in *)
  (* let visible_trees_cols = (List.fold (List.map effective_cols *)
  (*                                        ~f:visible_trees_count_hori_inner) *)
  (*                            ~init:0 *)
  (*                            ~f:(+)) in *)
  (* ExtLib.print visible_trees_cols; *)
  (* assert (visible_trees_cols = 5); *)
  
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
