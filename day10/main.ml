open! Core
open Stdio

let example = {|16
10
15
5
1
11
7
19
6
12
4
|}

let example2 = {|28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3
|}

let rec plug_all_adapters adapters target_joltage source (d1,d2,d3) =
  match adapters with
  | hd::tl ->
    let difference = hd-source in
    let differences = match difference with
      | 0 -> (d1, d2, d3)
      | 1 -> (d1+1, d2, d3)
      | 2 -> (d1, d2+1, d3)
      | 3 -> (d1, d2, d3+1)
      | _ -> failwith "no adapter available for this joltage"
    in
    plug_all_adapters tl target_joltage hd differences
  | [] -> (d1,d2,d3)

let can_plug_to target source =
  source = target || source + 1 = target || source + 2 = target || source + 3 = target

let count_possible_combinations' adapters source target =
  let cache = Hashtbl.create (module Int) in
  let rec count_possible_combinations adapters source target =
    match Hashtbl.find cache source with
    | Some n -> n
    | None -> match adapters with
      | [] -> if can_plug_to target source then 1 else 0
      | hd::hs ->
        let c = match can_plug_to hd source with
          | false -> 0
          | true -> (count_possible_combinations hs hd target) + (count_possible_combinations hs source target)
        in
        Hashtbl.set cache ~key:source ~data:c;
        c
  in
  count_possible_combinations adapters source target

let part_a input =
  let adapter_ratings = String.split_lines input |> List.map ~f:Int.of_string |> List.sort ~compare:Int.ascending in
  let max_joltage = List.max_elt adapter_ratings ~compare:Int.compare |> Option.value_exn in
  let device_joltage = max_joltage + 3 in
  let adapters = adapter_ratings @ [device_joltage] in
  let (diff1, diff2, diff3) = plug_all_adapters adapters device_joltage 0 (0,0,0) in
  print_s [%sexp (diff1, diff2, diff3 : (int * int * int))]

let part_b input =
  let adapter_ratings = String.split_lines input |> List.map ~f:Int.of_string |> List.sort ~compare:Int.ascending in
  let max_joltage = List.max_elt adapter_ratings ~compare:Int.compare |> Option.value_exn in
  let device_joltage = max_joltage + 3 in
  count_possible_combinations' adapter_ratings 0 device_joltage |> [%sexp_of: int] |> print_s

let () =
  let input = In_channel.read_all "input.txt" in
  (* part_a example2 *)
  part_b input
