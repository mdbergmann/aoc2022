open Base
open Stdio

let inspect x = ExtLib.print x; x

let make_range s e  =
  let rec range_fun s e lst =
    if s = e then lst
    else s :: range_fun (s+1) e lst
  in range_fun s e []

let print_index index =
  let (l, r) = index in
  print_string ("(" ^ (Int.to_string l) ^ "," ^ (Int.to_string r) ^ ")")

let print_indices lst =
  List.iter lst ~f:print_index;
  print_endline ""

let day_8 input =
  let lines = String.split_lines input in
  let rows = List.map lines ~f:(fun line ->
                 List.map (String.to_list line) ~f:(fun c ->
                     (Char.to_int c) - (Char.to_int '0'))) in
  let visible_trees_from_left_on_row row index =
    let recorded_indices = ref [] in
    let visible_elem = ref (List.nth_exn row 0) in
    List.iteri (List.drop_last_exn row) ~f:(fun i elem ->
        if elem > !visible_elem
        then (
          visible_elem := elem;
          recorded_indices := (index, i) :: !recorded_indices;
      ));
    !recorded_indices
  in

  let visible_trees_from_right_on_row row index =
    let recorded_indices = ref [] in
    let visible_elem = ref (List.last_exn row) in
    List.iteri (List.rev (List.drop row 1)) ~f:(fun i elem ->
        if elem > !visible_elem
        then (
          visible_elem := elem;
          let adjusted_index = (List.length row) - (i + 1) in
          recorded_indices := (index, adjusted_index) :: !recorded_indices;
      ));
    !recorded_indices
  in

  let visible_trees_row_inner row index =
    let l_indices = visible_trees_from_left_on_row row index in
    let r_indices = visible_trees_from_right_on_row row index in

    let new_r_indices = List.filter r_indices
                          ~f:(fun (row, col) ->
                            not (List.exists l_indices
                              ~f:(fun (rowl, coll) ->
                                row = rowl && col = coll
                              ))
                          ) in
    let indices = List.append l_indices new_r_indices in
    indices
  in

  (* let hori_count indices = List.length indices in *)
  
  (* assert (hori_count (visible_trees_row_inner (List.nth_exn rows 0) 0) = 1); *)
  (* assert (hori_count (visible_trees_row_inner (List.nth_exn rows 1) 1) = 2); *)
  (* assert (hori_count (visible_trees_row_inner (List.nth_exn rows 2) 2) = 2); *)
  (* assert (hori_count (visible_trees_row_inner (List.nth_exn rows 3) 3) = 1); *)
  (* assert (hori_count (visible_trees_row_inner (List.nth_exn rows 4) 4) = 2); *)
  
  let effective_rows = (List.drop_last_exn (List.drop rows 1)) in
  let visible_tree_indices_rows = List.fold (List.mapi effective_rows
                                               ~f:(fun i x ->
                                                 visible_trees_row_inner x i))
                                    ~init:[]
                                    ~f:List.append in
  (* let visible_trees_rows = List.length visible_tree_indices_rows in *)
  (* print_indices visible_tree_indices_rows; *)
  (* assert (visible_trees_rows = 5); *)

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
  
  let effective_cols = (List.drop_last_exn (List.drop cols 1)) in
  let visible_tree_indices_cols = List.fold (List.mapi effective_cols
                                                   ~f:(fun i x ->
                                                     visible_trees_row_inner x i))
                                        ~init:[]
                                        ~f:List.append in
  (* let visible_tree_indices_cols = List.map visible_tree_indices_cols_tmp *)
  (*                                   ~f:(fun (l_index, r_index) -> (r_index, l_index)) in *)
  
  (* print_indices visible_tree_indices_cols; *)
  (* let visible_trees_cols = List.length visible_tree_indices_cols in *)
  (* assert (visible_trees_cols = 3); *)

  let combined_indices = List.append (List.filter visible_tree_indices_cols
                                        ~f:(fun (row, col) ->
                                          not (List.exists visible_tree_indices_rows
                                                 ~f:(fun (rowl, coll) ->
                                                   row = rowl && col = coll
                                            ))
                           ))
                           visible_tree_indices_rows in
  (* print_indices combined_indices; *)
  let row_len = List.length rows in
  let complete_count = (List.length combined_indices) + 4 + (4 * (row_len - 2)) in
  ExtLib.print complete_count;
  complete_count

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

let prep_input =
  In_channel.read_all "/Users/mbergmann/Development/MySources/aoc2022/input/day8_1.txt"

let%test "day 8 - real test, 1" =
  let result = day_8 prep_input
  in
  printf "Result day_8 (real, 1): %s\n" (ExtLib.dump result);
  true
  (* result = 21 *)
