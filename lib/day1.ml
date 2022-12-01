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

let day_1 input =
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
  |> List.hd_exn

let%test "day 1 - demo test" =
  let result = day_1 test_input
  in
  printf "Result: %s\n" (ExtLib.dump result);
  result = 24000
