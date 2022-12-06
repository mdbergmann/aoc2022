open! Base
open Stdio

let inspect x = ExtLib.print x; x

let make_range s e  =
  let rec range_fun s e lst =
    if s = e then lst
    else s :: range_fun (s+1) e lst
  in range_fun s e []

exception Found_Marker of string

let day_6 input distinct_len =
  let acc = try String.fold (String.drop_suffix input 1)
                  ~init:""
                  ~f:(fun acc c ->
                    let new_acc =
                      if (String.length acc) < distinct_len then
                        acc ^ (String.of_char c)
                      else
                        if (List.contains_dup (String.to_list acc) ~compare:Char.compare) then
                          (String.drop_prefix acc 1) ^ (String.of_char c)
                        else raise (Found_Marker acc) in
                    new_acc
                  ) with
            | Found_Marker acc -> acc
            | _ -> assert false in
  ExtLib.print acc;
  let found_index = (String.substr_index_exn input ~pattern:acc) in
  ExtLib.print found_index;
  let add_additional = if distinct_len = 4 then 4 else 14 in
  add_additional+found_index

let%test "day 6 - demo test, 1" =
  let result = day_6 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 4
  in
  printf "Result day_6 (demo, 1): %s\n" (ExtLib.dump result);
  result = 7

(* let%test "day 6-2 - demo test, 1" = *)
(*   let result = day_6 "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 14 *)
(*   in *)
(*   printf "Result day_6-2 (demo, 1): %s\n" (ExtLib.dump result); *)
(*   result = 19 *)

let%test "day 6 - demo test, 2" =
  let result = day_6 "bvwbjplbgvbhsrlpgdmjqwftvncz" 4
  in
  printf "Result day_6 (demo, 2): %s\n" (ExtLib.dump result);
  result = 5

let%test "day 6-2 - demo test, 2" =
  let result = day_6 "bvwbjplbgvbhsrlpgdmjqwftvncz" 14
  in
  printf "Result day_6-2 (demo, 2): %s\n" (ExtLib.dump result);
  result = 23

let%test "day 6 - demo test, 3" =
  let result = day_6 "nppdvjthqldpwncqszvftbrmjlhg" 4
  in
  printf "Result day_6 (demo, 3): %s\n" (ExtLib.dump result);
  result = 6

let%test "day 6-2 - demo test, 3" =
  let result = day_6 "nppdvjthqldpwncqszvftbrmjlhg" 14
  in
  printf "Result day_6-2 (demo, 3): %s\n" (ExtLib.dump result);
  result = 23

let%test "day 6-2 - demo test, 4" =
  let result = day_6 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 14
  in
  printf "Result day_6-2 (demo, 4): %s\n" (ExtLib.dump result);
  result = 29

let prep_input =
  In_channel.read_all "/Users/mbergmann/Development/MySources/aoc2022/input/day6_1.txt"

let%test "day 6 - real test" =
  let result = day_6 prep_input 4
  in
  printf "Result day_6 (real): %s\n" (ExtLib.dump result);
  result = 1542

let%test "day 6-2 - real test" =
  let result = day_6 prep_input 14
  in
  printf "Result day_6-2 (real): %s\n" (ExtLib.dump result);
  result = 3153
