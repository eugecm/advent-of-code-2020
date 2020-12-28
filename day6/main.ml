open! Core
open Stdio

let group_separator = Re2.create_exn {|\n\n|}

let read_input () =
  In_channel.read_all "input.txt"

let parse_group s =
  String.chop_suffix_if_exists s ~suffix:"\n"
  |> String.split_lines
  |> List.map ~f:String.to_list

let group_unique_answers_anyone grp =
  Char.Hash_set.of_list (List.concat grp)

let part_a () =
  let group_parts = Re2.split group_separator (read_input ()) in
  let groups = List.map group_parts ~f:parse_group in
  let unique_answers = List.map groups ~f:group_unique_answers_anyone in
  let sum_unique_answers = List.fold unique_answers ~init:0 ~f:(fun a ans -> a + (Hash_set.length ans)) in
  print_s [%sexp (sum_unique_answers : int)]

let group_unique_answers_everyone grp =
  let answer_groups = List.map grp ~f:Char.Hash_set.of_list in
  List.fold answer_groups ~init:(List.nth_exn answer_groups 0) ~f:Hash_set.inter

let part_b () =
  let group_parts = Re2.split group_separator (read_input ()) in
  let groups = List.map group_parts ~f:parse_group in
  let unique_answers = List.map groups ~f:group_unique_answers_everyone in
  let sum_unique_answers = List.fold unique_answers ~init:0 ~f:(fun a ans -> a + (Hash_set.length ans)) in
  print_s [%sexp (sum_unique_answers : int)]

let () =
  (* part_a () *)
  part_b ()
