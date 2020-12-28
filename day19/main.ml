open! Core
open Stdio

let example0 ={|0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb|}

let example_2a = {|42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: "a"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: "b"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba
|}

let example_3 = {|1: "a"
2: "b"
0: 1 1 1 | 1 1 | 1

a
aa
aaa
aaaa|}

module Rule = struct
  type t =
    | Terminal of char
    | Composition of t list
    | Or of (t * t) [@@deriving sexp]

  let compose a b = Composition [a; b]

  let rec as_string t =
    match t with
    | Terminal c -> String.of_char c
    | Composition rules ->
      List.map rules ~f:as_string |> String.concat
    | Or (rule_left, rule_right) ->
      "(" ^ (as_string rule_left) ^ "|" ^ (as_string rule_right) ^ ")"

  let rec parse t cs =
    match t with
    | Terminal c -> (
        (* Debug.eprint_s [%message "TERMINAL RULE" (cs : char list) (c : char)]; *)
        match cs with
        | [] -> ([], cs)
        | hd::tail -> if Char.(=) hd c then ([hd], tail) else ([], cs)
      )
    | Composition rules ->
      (* Debug.eprint_s [%message "COMPOSITION RULE" (cs : char list) (rules : t list)]; *)
      List.fold_until rules ~init:([], cs) ~f:(fun (consumed, left) rule ->
          let matched, left = parse rule left in
          if List.is_empty matched then
            Stop ([], left)
          else
            Continue(consumed @ matched, left))
        ~finish:(Fn.id)
    | Or (rule_left, rule_right) ->
      let matched, left = parse rule_left cs in
      (* Debug.eprint_s [%message "OR RULE PARSE LEFT" (cs : char list) (rule_a : t) (matched : char list) (left : char list)]; *)
      if List.is_empty matched then parse rule_right cs else matched, left

  let load_rules st =
    let module Rule_or_definition = struct
      type rod =
        | Rule of t
        | Definition of string list
    end in

    let lines = String.split_lines st in
    let parsed_lines = List.map lines ~f:(fun line ->
        let parts = String.split line ~on:':' in
        match parts with
        | index::definition::[] ->
          let index' = Int.of_string index in
          let def_tokens = String.chop_prefix_if_exists definition ~prefix:" " |> String.split ~on:' ' in
          (index', Rule_or_definition.Definition def_tokens)
        | _ -> failwith "invalid rule definition"
      )
    in
    let definitions = Hashtbl.of_alist_exn (module Int) parsed_lines in

    let rec resolve i =
      let r = Hashtbl.find_exn definitions i in
      let open Rule_or_definition in
      match r with
      | Rule rule -> rule
      | Definition st ->
        if List.exists st ~f:(String.is_prefix ~prefix:{|"|}) then (* Case is terminal *)
          let rule = Terminal (String.get (List.nth_exn st 0) 1) in
          Hashtbl.set definitions ~key:i ~data:(Rule rule);
          rule
        else if not @@ List.exists st ~f:(String.(=) "|") then (* Case composition of rules *)
          let rules = List.map st ~f:(fun rule_idx ->
              let idx = Int.of_string rule_idx in
              resolve idx)
          in
          let rule = Composition rules in
          Hashtbl.set definitions ~key:i ~data:(Rule rule);
          rule
        else (* OR case *)
          let sides = List.group st ~break:(fun _ b -> String.(=) b "|") in
          let left_rules_defs = List.hd_exn sides in
          let rest = List.map (List.tl_exn sides) ~f:(fun rs -> List.tl_exn rs) in
          let right_rules_defs = List.nth_exn rest 0 in
          let left_rules = List.map left_rules_defs ~f:(fun rule_idx ->
              let idx = Int.of_string rule_idx in
              resolve idx)
          in
          let right_rules = List.map right_rules_defs ~f:(fun rule_idx ->
              let idx = Int.of_string rule_idx in
              resolve idx)
          in
          if List.length rest = 1 then (
            let rule = Or (Composition left_rules, Composition right_rules) in
            Hashtbl.set definitions ~key:i ~data:(Rule rule);
            rule
          ) else (
            let rule_group = [Composition left_rules; Composition right_rules] in
            let rest_rule_groups = List.map (List.tl_exn rest) ~f:(fun rdefs ->
                Composition (List.map rdefs ~f:(fun rule_idx -> resolve (Int.of_string rule_idx))))
            in
            let rec or_compose rules =
              match rules with
              | [] -> failwith "invalid zero elems"
              | [_] -> failwith "invalid 1 elem"
              | a::b::[] -> Or (a, b)
              | hd::tl -> Or(hd, or_compose tl)
            in
            let rule = or_compose (rule_group @ rest_rule_groups) in
            Hashtbl.set definitions ~key:i ~data:(Rule rule);
            rule
          )
    in

    List.iter (Hashtbl.keys definitions) ~f:(fun i -> ignore(resolve i : t));

    Hashtbl.map definitions ~f:(fun v ->
        match v with
        | Rule_or_definition.Definition _ -> failwith "unexpected definition. should have resolved to rule"
        | Rule_or_definition.Rule r -> r)
end

module Rule2 = struct

  let terminal_rule_re = Re2.create_exn {|^(\d+): "([a-z])"$|}
  let chain_rule_re = Re2.create_exn {|^(\d+): ([0-9 ]+)$|}
  let or_rule_re = Re2.create_exn {|^(\d+): ([0-9 ]+)\| ([0-9 ]+)$|}

  type t = 
    | Terminal of string
    | Chain of int list
    | Or of (int list * int list)
  [@@deriving sexp]

  let parse s : (int * t) =
    match (Re2.find_submatches terminal_rule_re s,
           Re2.find_submatches chain_rule_re s,
           Re2.find_submatches or_rule_re s) with
    | (Ok matches, _, _) ->
      let index = Array.get matches 1 |> Option.value_exn |> Int.of_string in
      let term = Array.get matches 2 |> Option.value_exn in
      index, Terminal term
    | (_, Ok matches, _) ->
      let index = Array.get matches 1 |> Option.value_exn |> Int.of_string in
      let rs = Array.get matches 2 |> Option.value_exn |> String.split ~on:' ' |> List.map ~f:Int.of_string in
      index, Chain rs
    | (_, _, Ok matches) ->
      let index = Array.get matches 1 |> Option.value_exn |> Int.of_string in
      let ls = Array.get matches 2 |> Option.value_exn |> String.strip |> String.split ~on:' ' |> List.map ~f:Int.of_string in
      let rs = Array.get matches 3 |> Option.value_exn |> String.strip |> String.split ~on:' ' |> List.map ~f:Int.of_string in
      index, Or (rs, ls)
    | _ -> failwithf "could not parse rule: '%s'" s ()

  let load_rules lines = Map.of_alist_exn (module Int) (List.map lines ~f:parse)

  let matches db index st =
    let rec loop i st = (* Returns the unmatched part *)
      (* Debug.eprint_s [%message "loop" (i : int) (st : string)]; *)
      let compose rules st =
        List.fold_until rules ~init:st ~f:(fun left rule ->
            (* Debug.eprint_s [%message "running rule" (rule : int) (left : string)]; *)
            let unmatched = loop rule left in
            (* Debug.eprint_s [%message "composed" (rules : int list) (rule : int) (left : string) (unmatched : string)]; *)
            if String.equal unmatched left then (*did not match *) (
              (* Debug.eprint_s [%message "stopped" (rules : int list) (rule : int) (left : string) (unmatched : string)]; *)
              Continue_or_stop.Stop st )
            else (
              (* Debug.eprint_s [%message "continue" (rules : int list) (rule : int) (left : string) (unmatched : string)]; *)
              Continue_or_stop.Continue unmatched))
          ~finish:(Fn.id)
      in
      let rule = Map.find_exn db i in
      match rule with
      | Terminal term ->
        (* Debug.eprint_s [%message "terminal" (st : string) (rule : t) (term : string)]; *)
        if String.is_prefix st ~prefix:term then (String.drop_prefix st 1) else st
      | Chain rules ->
        (* Debug.eprint_s [%message "chain" (st : string) (rule : t) (rules : int list)]; *)
        compose rules st
      | Or (left_rules, right_rules) ->
        (* Debug.eprint_s [%message "or" (st : string) (rule : t) (left_rules : int list) (right_rules : int list)]; *)
        let unmatched_left = compose left_rules st in
        if String.equal unmatched_left st then (* did not match *)
          compose right_rules st
        else
          unmatched_left
    in
    String.is_empty (loop index st)

  let rec resolve db i =
    let compose rules =
      match rules with
      | [] -> failwith "can't chain empty rules"
      | [r] -> resolve db r
      | r::rest ->
        List.fold rest ~init:(resolve db r) ~f:(fun acc rule ->
            List.concat_map (resolve db rule) ~f:(fun suffix ->
                List.map acc ~f:(fun prefix -> prefix ^ suffix)))
    in
    let rule = Map.find_exn db i in
    match rule with
    | Terminal term -> [term]
    | Chain rules -> compose rules
    | Or (left_rules, right_rules) -> List.concat[(compose left_rules);(compose right_rules)]

  let rec resolve_patched db i =
    let compose rules =
      match rules with
      | [] -> failwith "can't chain empty rules"
      | [r] -> resolve db r
      | r::rest ->
        List.fold rest ~init:(resolve_patched db r) ~f:(fun acc rule ->
            List.concat_map (resolve_patched db rule) ~f:(fun suffix ->
                List.map acc ~f:(fun prefix -> prefix ^ suffix)))
    in
    if i = 8 then begin
      Debug.eprint_s [%message "running 8 patch"];
      resolve db 42
    end
    else if i = 11 then begin
      Debug.eprint_s [%message "running 11 patch"];
      (compose [42;31])
    end
    else if i = 999 then begin
      (compose [42;42;31]) @ (compose [42;42;42;31;31]) @ (compose [42;42;42;42;31;31;31])
    end
    else begin
      let rule = Map.find_exn db i in
      match rule with
      | Terminal term -> [term]
      | Chain rules -> compose rules
      | Or (left_rules, right_rules) -> List.concat [(compose left_rules) ; (compose right_rules)]
    end
end


let part_a input =
  let groups = String.split_lines input |> List.group ~break:(fun _ b -> String.is_empty b) in
  let rules = Rule.load_rules (String.concat ~sep:"\n" (List.nth_exn groups 0)) in
  let messages = List.nth_exn groups 1 |> List.tl_exn in
  let _result = List.count messages ~f:(fun message ->
      let rule = Hashtbl.find_exn rules 0 in
      let matched, _left = Rule.parse rule (String.to_list message) in
      (List.length matched = String.length message))
  in
  print_s [%sexp (_result : int)];

let rec chunks st len =
  if String.is_empty st then [] else
  (String.sub st ~pos:0 ~len) :: (chunks (String.sub st ~pos:len ~len:(String.length st - len)) len)

let _part_b _input =
  let groups = String.split_lines _input |> List.group ~break:(fun _ b -> String.is_empty b) in
  let rules = List.nth_exn groups 0 |> Rule2.load_rules in
  let messages = List.nth_exn groups 1 |> List.tl_exn in
  let rule42_strings = Rule2.resolve rules 42 |> String.Set.of_list in
  let rule31_strings = Rule2.resolve rules 31 |> String.Set.of_list in
  let answer = List.count messages ~f:(fun msg ->
      let is_42 = Set.mem rule42_strings in
      let is_31 = Set.mem rule31_strings in
      let chunked = chunks msg (String.length (Set.choose_exn rule42_strings)) in
      let n_matches_31 = List.take_while (List.rev chunked) ~f:is_31 |> List.length in
      let n_matches_42 = List.take_while chunked ~f:is_42 |> List.length in
      (n_matches_42 > n_matches_31) && (n_matches_31 > 0) && (n_matches_42 + n_matches_31 = List.length chunked)
    )
  in
  print_s [%sexp (answer :int)]

let () =
  let _input = In_channel.read_all "input.txt" in
  (* part_a example_2a *)
  _part_b _input

(* It's not 344 *)
(* It's not 327 *)
(* It's not 315 *)
(* It's not 323 *)
    314?
