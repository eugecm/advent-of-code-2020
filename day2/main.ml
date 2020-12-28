open! Core
open Stdio

type policy = {
  a: int;
  b: int;
  c: char;
}

let policy_line_pattern = Re2.create_exn {|^(\d+)-(\d+) ([a-z]): ([a-z]+)$|}

let parse_db_line line =
  let parts = Re2.find_submatches_exn policy_line_pattern line in
  let p = {
    a=Int.of_string (Option.value_exn parts.(1));
    b=Int.of_string (Option.value_exn parts.(2));
    c=Char.of_string (Option.value_exn parts.(3));
  } in
  (p, Option.value_exn parts.(4))

let is_valid_password policy password =
  let char_count = Array.count (String.to_array password) ~f:(fun c ->
      Char.equal c (policy.c)) in
  char_count >= policy.a && char_count <= policy.b

let run policy_validator () =
  let policy_passwords =
    In_channel.read_lines "input.txt"
    |> List.map ~f:parse_db_line in
  let valid_entries = List.count policy_passwords
      ~f:(fun (policy, password) -> policy_validator policy password) in
  printf "%d\n" valid_entries

let is_valid_password_part_b policy password =
  let password_chars = String.to_array password in
  let open Char in
  ((password_chars.(policy.a-1) = policy.c) && 
  (password_chars.(policy.b-1) <> policy.c)) ||
  ((password_chars.(policy.a-1) <> policy.c) && 
  (password_chars.(policy.b-1) = policy.c))

let () =
  (* run is_valid_password () *)
  run is_valid_password_part_b ()
