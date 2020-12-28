open! Core
open Stdio

let part_a () =
  let want = Int.Hash_set.create () in
  let lines = In_channel.read_lines "input.txt" in
  let num = List.find_map lines ~f:(fun num_str ->
      let n = Int.of_string num_str in
      if Hash_set.mem want n then Some n
      else (
        Hash_set.add want (2020-n);
        Hash_set.add want (2020-n);
        None
      ))
  in
  match num with
  | None -> failwith "did not find number"
  | Some n ->
    printf "%d\n" (n*(2020-n))

let part_b () =
  let numbers = In_channel.read_lines "input.txt"
                |> List.map ~f:Int.of_string
                |> Int.Hash_set.of_list
  in
  let nums = Hash_set.find_map numbers ~f:(fun a ->
      Hash_set.find_map numbers ~f:(fun b ->
          let c = 2020 - a - b in
          if Hash_set.mem numbers c then
            Some (a, b, c)
          else
            None))
  in
  match nums with
  | None -> failwith "did not find numbers"
  | Some (a, b, c) -> printf "%d\n" (a*b*c)
  
let () =
  (* part_a () *)
  part_b ()
