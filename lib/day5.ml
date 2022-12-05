open! Base
open Stdio
open! Str

let inspect x = ExtLib.print x; x

let make_range s e  =
  let rec range_fun s e lst =
    if s = e then lst
    else s :: range_fun (s+1) e lst
  in range_fun s e []

let day_5 input =
  let lines = String.split_lines input in
  let (crates_numbers_line_index, crates_numbers_line) = match
      List.findi lines ~f:(fun _ line -> not (String.contains line '[')) with
    | Some (l_i, line) -> (l_i, line)
    | None -> assert false in
  ExtLib.print (crates_numbers_line_index, crates_numbers_line);
  let stack_lines = List.sub lines ~pos:0 ~len:crates_numbers_line_index in
  let move_lines = List.drop lines (crates_numbers_line_index+2) in
  ExtLib.print stack_lines;
  ExtLib.print move_lines;
  let four_cols = ((String.length crates_numbers_line) - 4) / 4 in
  let col_indices = 1 :: List.append
                           (List.map ~f:(fun c -> 1 + (c * 4))
                              (make_range 1 (four_cols+1)))
                           [String.length crates_numbers_line-2] in
  let col_stacks = List.map col_indices ~f:(fun i -> (i, Stack.create())) in
  ExtLib.print col_stacks;
  List.iter (List.rev stack_lines) ~f:(fun stack_line ->
      List.iter col_stacks ~f:(fun (col_index, crate_stack) ->
          let crate_id = Char.to_string (String.get stack_line col_index) in
          if not (String.equal crate_id " ") then Stack.push crate_stack crate_id
        )
    );
  ExtLib.print col_stacks;
  
  let crate_stacks = List.map col_stacks ~f:(fun (_,stack) -> stack) in
  ExtLib.print (List.map crate_stacks ~f:Stack.top_exn);
  let move_ops = List.map move_lines ~f:(fun line ->
                     let matched = Str.string_match
                                     (Str.regexp "^move \\([0-9]+\\) from \\([0-9]+\\) to \\([0-9]+\\)")
                                     line
                                     0 in
                     if matched then ((Int.of_string (Str.matched_group 1 line)),
                                      (Int.of_string (Str.matched_group 2 line)),
                                      (Int.of_string (Str.matched_group 3 line)))
                     else assert false
                   ) in
  ExtLib.print move_ops;
  List.iter move_ops ~f:(fun (crate_count,from_stack_num,to_stack_num) ->
      for _ = 0 to (crate_count-1) do
        ExtLib.print (crate_count,from_stack_num,to_stack_num);
        let from_stack = List.nth_exn crate_stacks (from_stack_num-1) in
        let to_stack = List.nth_exn crate_stacks (to_stack_num-1) in
        let item = Stack.pop_exn from_stack in
        Stack.push to_stack item;
      done;
    );
  ExtLib.print crate_stacks;
  ExtLib.print (List.map crate_stacks ~f:Stack.top_exn);
  "CMZ"

let demo_input = "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"

let%test "day 5 - demo test" =
  let result = day_5 demo_input
  in
  printf "Result day_5 (demo): %s\n" (ExtLib.dump result);
  String.equal result "CMZ"

(* let prep_input = *)
(*   In_channel.read_all "/Users/mbergmann/Development/MySources/aoc2022/input/day5_1.txt" *)
