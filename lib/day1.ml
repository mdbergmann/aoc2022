open! Base
open Stdio

let test_input = "1000
2000
3000

4000

5000
6000

7000
8000
9000

10000"


let inspect x = ExtLib.print x; x

let day_1 input select_fun =
  let filter_empty_strings lst = List.filter lst ~f:(fun str -> not (String.is_empty str)) in
  let convert_to_int lst = List.map lst ~f:Int.of_string in
  let sum lst = List.fold lst ~init:0 ~f:(+) in
  let sort_desc = (fun a b -> if a > b then -1
                              else if a < b then 1
                              else 0) in
  input
  |> String.split ~on:'\n'
  |> List.group ~break:(fun a _ -> String.(=) a "")
  |> List.map ~f:filter_empty_strings
  |> List.map ~f:convert_to_int
  |> List.map ~f:sum
  |> List.sort ~compare:sort_desc
  |> select_fun

let%test "day 1 - demo test" =
  let result = day_1 test_input (fun lst -> List.hd_exn lst)
  in
  printf "Result 1: %s\n" (ExtLib.dump result);
  result = 24000

let prep_input =
  In_channel.read_all "/Users/mbergmann/Development/MySources/aoc2022/input/day1_1.txt"

let%test "day 1 - full test" =
  let result = day_1 prep_input (fun lst -> List.hd_exn lst)
  in
  printf "Result 1: %s\n" (ExtLib.dump result);
  result = 71300

let%test "day 1-2 - demo test" =
  let result = day_1 test_input (fun lst -> List.fold (List.take lst 3) ~init:0 ~f:(+))
  in
  printf "Result 1-2: %s\n" (ExtLib.dump result);
  result = 45000

let%test "day 1-2 - full test" =
  let result = day_1 prep_input (fun lst -> List.fold (List.take lst 3) ~init:0 ~f:(+))
  in
  printf "Result 1-2: %s\n" (ExtLib.dump result);
  true
