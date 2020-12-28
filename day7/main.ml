open! Core
open Stdio


type fit = {
  n: int;
  bag: string;
} [@@deriving sexp]

type bag = {
  name: string;
  connections: fit list;
} [@@deriving sexp]

let example = {|light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.|}

let parse_rule s : (string * (int * string) list) =
  match String.split s ~on:' ' with
  | c0::c1::_::"contain"::"no"::"other"::_::[] ->
    let name = c0 ^ " " ^ c1 in
    (name, [])
  | c0::c1::_::"contain"::d::c2::c3::_::rest ->
    let rec parse rest =
      match rest with
      | [] -> []
      | qty::c0::c1::_::rest ->
        let r = (Int.of_string qty, c0 ^ " " ^ c1) in
        r::(parse rest)
      | _ -> failwith "invalid pattern"
    in
    (c0 ^ " " ^ c1, (Int.of_string d, c2 ^ " " ^ c3)::(parse rest))
  | _ -> failwith "invalid input"

let find_bag rule_map to_ =
  let reaches = Hash_set.create (module String) in
  let rec _find_bag rule_map from to_ =
    if Hash_set.mem reaches from then true else
      let connected_bags = Map.find_exn rule_map from in
      let can_reach = List.find connected_bags ~f:(fun (_, bag) ->
          _find_bag rule_map bag to_ || String.(=) bag to_)
      in
      match can_reach with
      | Some _ ->
        Hash_set.add reaches from;
        true
      | None -> false
  in
  Map.filter_keys rule_map ~f:(fun bag -> _find_bag rule_map bag to_) |> Map.keys

let rec count_bags rule_map bag =
  let other_bags = Map.find_exn rule_map bag in
  1 + (List.sum (module Int) other_bags ~f:(fun (qty, other) ->
      qty * (count_bags rule_map other)))

let part_a input =
  let rules = String.split_lines input |> List.map ~f:parse_rule in
  let rule_map = Map.of_alist_exn (module String) rules in
  let reaches = find_bag rule_map "shiny gold" in
  print_s [%sexp (List.length reaches : int)]

let part_b input =
  let rules = String.split_lines input |> List.map ~f:parse_rule in
  let rule_map = Map.of_alist_exn (module String) rules in
  print_s [%sexp ((count_bags rule_map "shiny gold") -1 : int)]

let () =
  (* part_a (In_channel.read_all "input.txt") *)
  part_b (In_channel.read_all "input.txt")
