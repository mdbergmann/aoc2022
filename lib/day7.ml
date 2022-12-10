open Base
open Stdio

let inspect x = ExtLib.print x; x

let make_range s e  =
  let rec range_fun s e lst =
    if s = e then lst
    else s :: range_fun (s+1) e lst
  in range_fun s e []

type fs_item =
  | File of string * int
  | Dir of string * fs_item list * fs_item option

let day_7 input =
  let cmds = String.split_lines input in
  ExtLib.print cmds;
  let rec gen_folder_tree root_dir curr_dir cmd_lines =
    match cmd_lines with
    | [] -> (match curr_dir with
            | Dir (_, _, Some parent_dir) -> parent_dir
            | _ -> root_dir)
    | cmd :: cmds_rest ->
       (match cmd with
        | "$ cd /" -> gen_folder_tree root_dir root_dir cmds_rest
        | "$ ls" -> gen_folder_tree root_dir curr_dir cmds_rest
        | cmd -> (match (String.split cmd ~on:' ') with
                 | "dir " :: dir_name :: _ ->
                    (match curr_dir with
                    | Dir(name, fs_items, parent) -> 
                       gen_folder_tree root_dir (Dir(name, (Dir(dir_name, [], Some curr_dir) :: fs_items), parent)) cmds_rest
                    | _ -> gen_folder_tree root_dir curr_dir cmds_rest)
                 | _ -> gen_folder_tree root_dir curr_dir cmds_rest))
  in
  let root_dir = Dir("/", [], None) in
  let folder_tree = gen_folder_tree root_dir root_dir cmds in
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
