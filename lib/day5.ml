open! Base
open Stdio
open! Str

let inspect x = ExtLib.print x; x

let make_range s e  =
  let rec range_fun s e lst =
    if s = e then lst
    else s :: range_fun (s+1) e lst
  in range_fun s e []

let prepare_crate_stacks_and_move_lines input =
  let lines = String.split_lines input in
  let (crates_numbers_line_index, crates_numbers_line) = match
      List.findi lines ~f:(fun _ line -> not (String.contains line '[')) with
    | Some (l_i, line) -> (l_i, line)
    | None -> assert false in
  let stack_lines = List.sub lines ~pos:0 ~len:crates_numbers_line_index in
  let move_lines = List.drop lines (crates_numbers_line_index+2) in
  let four_cols = ((String.length crates_numbers_line) - 4) / 4 in
  let col_indices = 1 :: List.append
                           (List.map ~f:(fun c -> 1 + (c * 4))
                              (make_range 1 (four_cols+1)))
                           [String.length crates_numbers_line-2] in
  let col_stacks = List.map col_indices ~f:(fun i -> (i, Stack.create())) in
  List.iter (List.rev stack_lines) ~f:(fun stack_line ->
      List.iter col_stacks ~f:(fun (col_index, crate_stack) ->
          let crate_id = Char.to_string (String.get stack_line col_index) in
          if not (String.equal crate_id " ") then Stack.push crate_stack crate_id
        )
    );
  let crate_stacks = List.map col_stacks ~f:(fun (_,stack) -> stack) in
  (crate_stacks, move_lines)

let prepare_move_ops move_lines = 
  List.map move_lines ~f:(fun line ->
      let matched = Str.string_match
                      (Str.regexp "^move \\([0-9]+\\) from \\([0-9]+\\) to \\([0-9]+\\)")
                      line
                      0 in
      if matched then ((Int.of_string (Str.matched_group 1 line)),
                       (Int.of_string (Str.matched_group 2 line)),
                       (Int.of_string (Str.matched_group 3 line)))
      else assert false
    )

let day_5 input crane_mover_fun =
  let (crate_stacks, move_lines) = prepare_crate_stacks_and_move_lines input in
  let move_ops = prepare_move_ops move_lines in
  List.iter move_ops ~f:(crane_mover_fun crate_stacks);
  let top_crates = List.map crate_stacks ~f:Stack.top_exn in
  String.concat top_crates

let demo_input = "    [D]    
[N] [C]    
[Z] [M] [P]
 1   2   3 

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2"

let cranemover_9000 crate_stacks =
  (fun (crate_count,from_stack_num,to_stack_num) ->
    let from_stack = List.nth_exn crate_stacks (from_stack_num-1) in
    let to_stack = List.nth_exn crate_stacks (to_stack_num-1) in
    for _ = 0 to (crate_count-1) do
      let item = Stack.pop_exn from_stack in
      Stack.push to_stack item;
    done;)

let cranemover_9001 crate_stacks = 
  (fun (crate_count,from_stack_num,to_stack_num) ->
    let from_stack = List.nth_exn crate_stacks (from_stack_num-1) in
    let to_stack = List.nth_exn crate_stacks (to_stack_num-1) in
    let popped = List.map (make_range 0 crate_count)
                   ~f:(fun _ -> Stack.pop_exn from_stack) in
    List.iter (List.rev popped) ~f:(Stack.push to_stack)
  )

let%test "day 5 - demo test" =
  let result = day_5 demo_input cranemover_9000
  in
  printf "Result day_5 (demo): %s\n" (ExtLib.dump result);
  String.equal result "CMZ"

let%test "day 5-2 - demo test" =
  let result = day_5 demo_input cranemover_9001
  in
  printf "Result day_5-2 (demo): %s\n" (ExtLib.dump result);
  String.equal result "MCD"

let prep_input =
  In_channel.read_all "/Users/mbergmann/Development/MySources/aoc2022/input/day5_1.txt"

let%test "day 5 - real test" =
  let result = day_5 prep_input cranemover_9000
  in
  printf "Result day_5 (real): %s\n" (ExtLib.dump result);
  String.equal result "FZCMJCRHZ"

let%test "day 5-2 - real test" =
  let result = day_5 prep_input cranemover_9001
  in
  printf "Result day_5-2 (real): %s\n" (ExtLib.dump result);
  String.equal result "JSDHQMZGF"
