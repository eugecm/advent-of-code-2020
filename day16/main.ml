open! Core
open Stdio

let example = {|class: 0-1 or 4-19
row: 0-5 or 8-19
seat: 0-13 or 16-19

your ticket:
11,12,13

nearby tickets:
3,9,18
15,1,5
5,14,9
|}

let rule_pattern = Re2.create_exn {|([a-z ]+): (\d+)-(\d+) or (\d+)-(\d+)|}

module Input = struct
  type rule = {
    name: string;
    r1: (int * int);
    r2: (int * int);
  }[@@deriving sexp, equal]

  type ticket = int list [@@deriving sexp]

  type t = {
    rules: rule list;
    my_ticket: ticket;
    nearby_tickets: ticket list;
  } [@@deriving sexp]

  let parse (s:string) : t =
    let groups = String.split_lines s |> List.group ~break:(fun a b -> (String.is_empty a) || (String.is_empty b)) in
    let rules_lines = List.nth_exn groups 0 in
    let my_ticket_lines = List.nth_exn groups 2 in
    let nearby_ticket_lines = List.nth_exn groups 4 in
    let rules = List.map rules_lines ~f:(fun l ->
        let groups = Re2.find_submatches_exn rule_pattern l in
        let name = Array.get groups 1 |> Option.value_exn in
        let from1 = Array.get groups 2 |> Option.value_exn |> Int.of_string in
        let to1 = Array.get groups 3 |> Option.value_exn |> Int.of_string in
        let from2 = Array.get groups 4 |> Option.value_exn |> Int.of_string in
        let to2 = Array.get groups 5 |> Option.value_exn |> Int.of_string in
        {name;r1=(from1,to1);r2=(from2,to2)})
    in
    let my_ticket = List.nth_exn my_ticket_lines 1 |> String.split ~on:',' |> List.map ~f:Int.of_string in
    let nearby_tickets = List.map (List.tl_exn nearby_ticket_lines) ~f:(fun line ->
        String.split line ~on:',' |> List.map ~f:Int.of_string)
    in
    {rules;my_ticket;nearby_tickets}

  let validate (rule:rule) num =
    Int.between num ~low:(fst rule.r1) ~high:(snd rule.r1) || Int.between num ~low:(fst rule.r2) ~high:(snd rule.r2)

  let invalid_fields rules ticket =
    let _, invalid_fields = List.fold ticket ~init:(rules, []) ~f:(fun (rules, invalid_fields) field ->
        match List.for_all rules ~f:(fun rule -> not @@ validate rule field) with
        | false -> (rules, invalid_fields)
        | true -> (rules, field::invalid_fields))
    in
    invalid_fields

  let discard_invalid rules tickets =
    List.filter tickets ~f:(fun ticket -> invalid_fields rules ticket |> List.length |> Int.(=) 0)

  let map_rules_to_positions rules valid_tickets =
    let possible_rules_per_field = List.mapi (List.nth_exn valid_tickets 0) ~f:(fun i _ ->
        let fields = List.map valid_tickets ~f:(fun ticket -> List.nth_exn ticket i) in
        List.filter rules ~f:(fun rule ->
            List.for_all fields ~f:(fun field -> validate rule field)))
    in
    let rec remove_possibilities (possibilities: rule list list) =
      if List.for_all possibilities ~f:(fun rules -> Int.(=) (List.length rules) 1) then
        possibilities
      else
        let only1 = List.filter_map possibilities ~f:(fun rules ->
            if Int.(=) 1 (List.length rules) then Some (List.hd_exn rules) else None)
        in
        let pruned = List.map possibilities ~f:(fun rules ->
            if Int.(=) (List.length rules) 1 then rules else
              List.filter rules ~f:(fun rule -> List.find only1 ~f:(fun o -> equal_rule o rule) |> Option.is_none))
        in
        remove_possibilities pruned
    in
    remove_possibilities possible_rules_per_field |> List.map ~f:(fun rules -> List.nth_exn rules 0)

end

let part_a input =
  let input = Input.parse input in
  let invalid_fields = List.concat_map (input.nearby_tickets) ~f:(fun ticket ->
      Input.invalid_fields input.rules ticket)
  in
  let error_rate = List.fold invalid_fields ~init:0 ~f:Int.(+) in
  print_s [%sexp (error_rate : int)]

let part_b input =
  let input = Input.parse input in
  let valid_tickets = Input.discard_invalid input.rules input.nearby_tickets in
  let rules = Input.map_rules_to_positions input.rules valid_tickets in
  let departure_fields = List.filter_mapi rules ~f:(fun i rule ->
      if String.is_prefix rule.name ~prefix:"departure" then
        Some i
      else None)
  in
  Debug.eprint_s [%sexp (departure_fields : int list)];
  let answer = List.fold departure_fields ~init:1 ~f:(fun acc field_i ->
      let field = List.nth_exn input.my_ticket field_i in
      acc * field)
  in
  print_s [%sexp (answer: int)]

let () =
  let input = In_channel.read_all "input.txt" in
  (* part_a input *)
  part_b input  
