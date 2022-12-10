open Base
open Stdio

let inspect x = ExtLib.print x; x

let make_range s e  =
  let rec range_fun s e lst =
    if s = e then lst
    else s :: range_fun (s+1) e lst
  in range_fun s e []

type fs_file = File of string * int
type fs_dir = Dir of string * fs_file list

let day_7 input =
  let cmds = String.split_lines input in
  ExtLib.print cmds;
  let gen_folder_tree curr_dir _parent_dir cmd_lines =
    match cmd_lines with
    | _ -> curr_dir in
  let folder_tree = gen_folder_tree (Dir("/", [])) None cmds in
  ExtLib.print folder_tree;
  95437

let demo_input = "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k"

let%test "day 7 - demo test, 1" =
  let result = day_7 demo_input
  in
  printf "Result day_6 (demo, 1): %s\n" (ExtLib.dump result);
  result = 95437

(* let prep_input = *)
(*   In_channel.read_all "/Users/mbergmann/Development/MySources/aoc2022/input/day6_1.txt" *)
