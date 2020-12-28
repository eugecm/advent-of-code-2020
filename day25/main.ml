open! Core
open Stdio

let example = {|5764801
17807724|}

let transform ?(start=1) sn ~size =
  let v = ref start in
  for _=1 to size do
    v := !v * sn;
    v := Int.rem !v 20201227
  done;
  !v

let find_loop_size num =
  let rec find cur target i =
    if cur = target then i else (
      (* Debug.eprint_s [%message "trying..." (cur : int) (i : int) (target : int)]; *)
      find (transform 7 ~start:cur ~size:1) target (i+1))
  in
  find 1 num 0

let part_a input =
  let pks = String.split_lines input |> List.map ~f:Int.of_string in
  let target = List.nth_exn pks 0 in
  let loop_size = find_loop_size target in
  let other_key = List.nth_exn pks 1 in
  let encryption_key = transform other_key ~size:loop_size in
  print_s [%message "broken" (target : int) (loop_size : int) (other_key : int) (encryption_key : int)]

let _part_b _input =
  ()

let () =
  let _input = In_channel.read_all "input.txt" in
  part_a _input
  (* part_b example *)  
