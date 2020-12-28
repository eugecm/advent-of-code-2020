open! Core
open Stdio

let example = {|939
7,13,x,x,59,x,31,19|}

module Rule = struct
  type t = {bus: int; offset_minutes: int} [@@deriving sexp, equal]

  let rules_of_string s =
    String.split s ~on:','
    |> List.filter_mapi ~f:(fun i bus ->
        if String.(=) bus "x" then None else
          Some {bus=(Int.of_string bus); offset_minutes=i})
end

let part_a input =
  let i = String.split_lines input in
  let depart_at = List.nth_exn i 0 |> Int.of_string in
  let buses = List.nth_exn i 1 |> String.split ~on:',' |> List.filter_map ~f:(fun bus -> if String.(=) bus "x" then None else Some (Int.of_string bus)) in
  let next_arrivals = List.map buses ~f:(fun bus ->
      let next_arrival = depart_at - (depart_at % bus) + bus in
      (bus, next_arrival))
  in
  let (next_bus, arrival) = List.min_elt next_arrivals ~compare:(fun a b -> Int.compare (snd a) (snd b)) |> Option.value_exn in
  print_s [%sexp (next_bus * (arrival-depart_at) : int)]

let timestamp_matches_rules timestamp (rules: Rule.t list) =
  List.for_all rules ~f:(fun rule -> (timestamp + rule.offset_minutes)%rule.bus = 0)

let rec find_timestamp_aux rules ~start ~stride =
  (* Debug.eprint_s [%sexp (start, stride, rules : int * int * Rule.t list)]; *)
  if timestamp_matches_rules start rules then start else
    find_timestamp_aux rules ~start:(start+stride) ~stride

let find_timestamp_with_inc rules ~start ~stride =
  let a = find_timestamp_aux rules ~start ~stride in
  let stride' = List.fold rules ~init:1 ~f:(fun acc rule -> acc * rule.bus) in
  (a, stride')

let find_timestamp (rules:Rule.t list) =
  let hd = List.hd_exn rules in
  let tl = List.tl_exn rules in
  let (_, timestamp, _) = List.fold tl ~init:([hd], 0, hd.bus) ~f:(fun (rules, timestamp, stride) next ->
      let rules' = rules @ [next] in
      let (timestamp', stride') = find_timestamp_with_inc rules' ~start:timestamp ~stride in
      Debug.eprint_s [%sexp (timestamp', stride', rules : int* int * (Rule.t list))];
      (rules', timestamp', stride'))
  in
  timestamp

let part_b input =
  let rules = Rule.rules_of_string input in
  let timestamp = find_timestamp rules in
  print_s [%sexp (timestamp : int)]

let () =
  let input = In_channel.read_all "input.txt" in
  (* part_a input *)
  (* let input = "\n1789,37,47,1889" in *)
  part_b (List.nth_exn (String.split_lines input) 1)
