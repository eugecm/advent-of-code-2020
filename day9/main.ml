open! Core
open Stdio

let example = {|35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576|}

let is_sum_of_preamble arr from len n =
  let preamble_base = List.range (from-len) from in
  let idxs = List.cartesian_product preamble_base preamble_base in
  List.find idxs ~f:(fun (i, j) -> i <> j && (arr.(i) + arr.(j)) = n)

let find_offending_number preamble_size nums =
  Array.findi nums ~f:(fun i n ->
      if i < preamble_size then false else
        is_sum_of_preamble nums i preamble_size n |> Option.is_none)

let rec find_sum l n i s =
  match l with
  | [] -> (0, false)
  | hd::hs -> if (i+hd) = n then (s+1, true) else (
      if (i+hd) > n then (0, false) else find_sum hs n (i+hd) (s+1)
    ) 

let find_contiguous_sum nums n =
  let length = Array.length nums in
  let l = Array.to_list nums in
  let (pos, len) = List.find_mapi l ~f:(fun i _ ->
      let (len, found) = find_sum (List.sub l ~pos:i ~len:(length - i - 1)) n 0 0 in
      if found then Some (i, len) else None
    ) |> Option.value_exn in
  Array.sub nums ~pos ~len

let part_a input =
  let numbers = String.split_lines input |> Array.of_list_map ~f:Int.of_string in
  find_offending_number 25 numbers |> [%sexp_of: (int * int) option] |> print_s

let part_b input =
  let numbers = String.split_lines input |> Array.of_list_map ~f:Int.of_string in
  let (_, num) = find_offending_number 25 numbers |> Option.value_exn in
  let range = find_contiguous_sum numbers num in
  let min = Array.min_elt range ~compare:Int.compare |> Option.value_exn in
  let max = Array.max_elt range ~compare:Int.compare |> Option.value_exn in
  print_s [%sexp (min + max : int)]

let () =
  let input = In_channel.read_all "input.txt" in
  (* part_a input *)
  part_b input
