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


let day_1 = 0

let%test "day 1 - demo test" =
  let result = day_1
  in
  printf "Result: %s\n" (ExtLib.dump result);
  result = 0
