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
  let rec find_root_dir dir =
    match dir with
    | Dir(_, _, Some parent) -> find_root_dir parent
    | _ -> dir in
  let rec gen_folder_tree curr_dir cmd_lines =
    match cmd_lines with
    | [] -> (match curr_dir with
            | Dir (_, _, Some parent_dir) -> parent_dir
            | dir -> find_root_dir dir)
    | cmd :: cmds_rest ->
       ExtLib.print (cmd, curr_dir);
       (match cmd with
        | "$ ls" -> gen_folder_tree curr_dir cmds_rest
        | cmd when String.is_prefix cmd ~prefix:"$ cd " ->
           (match (String.split cmd ~on:' ') with
            | [_; _; "/"] ->
               gen_folder_tree (find_root_dir curr_dir) cmds_rest
            | [_; _; ".."] ->
               (match curr_dir with
               | Dir(_, _, Some parent_dir) ->
                  gen_folder_tree parent_dir cmds_rest
               | _ ->
                  gen_folder_tree curr_dir cmds_rest)
            | [_; _; dir_name] ->
               (match curr_dir with
                | Dir(_, dir_items, _) ->
                   (let new_curr_dir_opt = List.find dir_items
                                        ~f:(fun item ->
                                          match item with
                                          | Dir(name, _, _) -> (String.equal name dir_name)
                                          | _ -> false) in
                   match new_curr_dir_opt with
                   | Some new_curr_dir -> 
                      gen_folder_tree new_curr_dir cmds_rest
                   | _ ->
                      gen_folder_tree curr_dir cmds_rest)
                | _ -> assert false)
            | _ -> 
               gen_folder_tree curr_dir cmds_rest
           )
        | cmd when String.is_prefix cmd ~prefix:"dir" ->
           (match (String.split cmd ~on:' ') with
            | ["dir"; new_dir_name] ->
               (match curr_dir with
                | Dir(dir_name, fs_items, parent_dir) ->
                   let new_fs_items = Dir(new_dir_name, [], Some curr_dir) :: fs_items in
                   let new_curr_dir = Dir(dir_name, new_fs_items, parent_dir) in
                   gen_folder_tree new_curr_dir cmds_rest
                | _ -> assert false)
            | _ -> gen_folder_tree curr_dir cmds_rest)
        | _ -> gen_folder_tree curr_dir cmds_rest)
  in
  let root_dir = Dir("/", [], None) in
  let folder_tree = gen_folder_tree root_dir cmds in
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
