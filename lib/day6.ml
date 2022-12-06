open! Base
open Stdio

let inspect x = ExtLib.print x; x

let make_range s e  =
  let rec range_fun s e lst =
    if s = e then lst
    else s :: range_fun (s+1) e lst
  in range_fun s e []

exception Found_Marker of string

let day_6 input =
  let acc = try String.fold input
                  ~init:""
                  ~f:(fun acc c ->
                    let new_acc =
                      if (String.length acc) < 4 then
                        acc ^ (String.of_char c)
                      else
                        if (String.contains acc c) ||
                             (List.contains_dup (String.to_list acc) ~compare:Char.compare) then
                          (String.drop_prefix acc 1) ^ (String.of_char c)
                        else raise (Found_Marker acc) in
                    ExtLib.print (acc, new_acc, (String.of_char c));
                    new_acc
                  ) with
            | Found_Marker acc -> acc
            | _ -> assert false in
  ExtLib.print acc;
  4+(String.substr_index_exn input ~pattern:acc)

let%test "day 6 - demo test, 1" =
  let result = day_6 "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
  in
  printf "Result day_6 (demo, 1): %s\n" (ExtLib.dump result);
  result = 7

(* let prep_input = *)
(*   In_channel.read_all "/Users/mbergmann/Development/MySources/aoc2022/input/day5_1.txt" *)

