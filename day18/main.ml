open! Core
open Stdio

let example = {|1 + (2 * 3) + (4 * (5 + 6))|}

module Math = struct
  module Operation = struct
    module T = struct
      type t =
        | Add
        | Multiply [@@deriving sexp, compare]
    end
    include T
    include Comparable.Make(T)
  end

  module Parens = struct
    module T = struct
      type t =
        | Open
        | Close [@@deriving sexp, compare]
    end
    include T
    include Comparable.Make(T)
  end

  module Token = struct
    module T = struct
      type t =
        | Number of int
        | Operator of Operation.t
        | Parens of Parens.t
      [@@deriving sexp, compare]
    end
    include T
    include Comparable.Make(T)
  end

  module SyntaxTree = struct
    type t =
      | Leaf of int
      | Tree of (t * Operation.t * t)
    [@@deriving sexp, compare]
  end

  let rec tokenize st =
    if String.is_empty st then [] else
      let parts = String.split st ~on:' ' in
      List.concat_map parts ~f:(fun p ->
          let prefix = String.sub p ~pos:0 ~len:1 in
          let tail = if String.length p > 1 then
              String.sub p ~pos:1 ~len:(String.length p -1)
            else
              ""
          in
          match prefix with
          | "(" -> Token.Parens Parens.Open :: (tokenize tail)
          | ")" -> Token.Parens Parens.Close :: (tokenize tail)
          | "*" -> Token.Operator Operation.Multiply :: (tokenize tail)
          | "+" -> Token.Operator Operation.Add :: (tokenize tail)
          | n -> Number (Int.of_string n) :: (tokenize tail))

  let find_closing_parens tokens =
    let rec loop tokens' i level =
      match tokens' with
      | [] ->
        failwiths ~here:[%here] "unbalanced parens" (tokens, i, level) [%sexp_of: (Token.t list * int * int)]
      | Token.Parens Parens.Close::rest ->
        (if level = 0 then i else loop rest (i+1) (level-1))
      | Token.Parens Parens.Open::rest ->
        loop rest (i+1) (level+1)
      | _::rest -> loop rest (i+1) level
    in
    loop tokens 0 0

  let rec parse tokens : SyntaxTree.t =
    match tokens with
    | [] -> failwith "cannot parse empty tokens"
    | [Token.Number a] -> SyntaxTree.Leaf a
    | Token.Number a :: Token.Operator op :: Token.Number b :: rest -> (
        let t = SyntaxTree.Tree (SyntaxTree.Leaf a, op, SyntaxTree.Leaf b) in
        match rest with
        | [] -> t
        | Token.Operator op :: rest' -> SyntaxTree.Tree (t, op, parse rest')
        | _ -> failwiths ~here:[%here] "invalid syntax" (t, rest) [%sexp_of: (SyntaxTree.t * Token.t list)]
      )
    | Token.Number a :: Token.Operator op :: (Token.Parens Parens.Open :: _ as rest) ->
      SyntaxTree.Tree (SyntaxTree.Leaf a, op, parse rest)
    | Token.Parens Parens.Open :: rest ->
      let closing_i = find_closing_parens rest in
      let expr_tree = parse (List.sub rest ~pos:0 ~len:closing_i) in
      let is_last_expr = (List.length rest = closing_i + 1) in
      if is_last_expr then
        expr_tree
      else (
        let after_parens = List.sub rest ~pos:(closing_i+1) ~len:(List.length rest - closing_i -1) in
        match after_parens with
        | Token.Operator op :: rest' -> SyntaxTree.Tree (expr_tree, op, parse rest')
        | _ -> failwiths ~here:[%here] "invalid syntax" (expr_tree, after_parens) [%sexp_of: (SyntaxTree.t * Token.t list)]
      )
    | _ -> failwiths ~here:[%here] "invalid syntax" (tokens) [%sexp_of: (Token.t list)]

  let rec parse2 tokens : int =
    let apply op a b =
      (* Debug.eprint_s [%message "OP A B" (op:Operation.t) (a:int) (b:int)]; *)
      match op with
      | Operation.Add -> a + b
      | Operation.Multiply -> a * b
    in
    match tokens with
    | [] -> failwith "cannot parse empty tokens"
    | [Token.Number a] -> a
    | Token.Number a :: Token.Operator op :: Token.Number b :: rest -> (
        (* Debug.eprint_s [%message "NUM OP NUM REST" (a : int) (op : Operation.t) (b :int) (rest : Token.t list)]; *)
        parse2 @@ Token.Number (apply op a b)::rest
      )
    | Token.Number a :: Token.Operator op :: Token.Parens Parens.Open :: after_open -> (
        let closing_paren_i = find_closing_parens after_open in
        let paren_expr_result = parse2 (List.sub after_open ~pos:0 ~len:closing_paren_i) in
        let res = apply op a paren_expr_result in
        let is_last_expr = (List.length after_open = closing_paren_i + 1) in
        if is_last_expr then
          res
        else
          let next_expr = List.sub after_open ~pos:(closing_paren_i+1) ~len:(List.length after_open - closing_paren_i -1) in
          parse2 @@ Token.Number res :: next_expr
      )
    | Token.Parens Parens.Open :: after_open -> (
        let closing_paren_i = find_closing_parens after_open in
        let res = parse2 (List.sub after_open ~pos:0 ~len:closing_paren_i) in
        let is_last_expr = (List.length after_open = closing_paren_i + 1) in
        if is_last_expr then
          res
        else
          let next_expr = List.sub after_open ~pos:(closing_paren_i+1) ~len:(List.length after_open - closing_paren_i -1) in
          parse2 @@ Token.Number res :: next_expr
      )
    | _ -> failwiths ~here:[%here] "invalid syntax" (tokens) [%sexp_of: (Token.t list)]

  let take_par_expr tokens =
    let closing_paren_i = find_closing_parens tokens in
    let expr = List.sub tokens ~pos:0 ~len:closing_paren_i in
    let is_last_expr = (List.length tokens = closing_paren_i + 1) in
    if is_last_expr then
        (expr, [])
    else
        let next_expr = List.sub tokens ~pos:(closing_paren_i+1) ~len:(List.length tokens - closing_paren_i -1) in
        (expr, next_expr)

  let rec parse_expr tokens =
    let result, rest = parse_term tokens in
    match rest with
    | Token.Operator Operation.Multiply :: rest' ->
      let rhs, rest'' = parse_expr rest' in
      (result * rhs, rest'')
    | other -> (result, other)
  and parse_term tokens =
    let result, rest = parse_item tokens in
    match rest with
    | Token.Operator Operation.Add :: rest' ->
      let rhs, rest'' = parse_term rest' in
      (result + rhs, rest'')
    | other -> (result, other)
  and parse_item tokens =
    match tokens with
    | Token.Number n::rest -> (n, rest)
    | Token.Parens Parens.Open :: rest' ->
      let par_expr, rest'' = take_par_expr rest' in
      (* Debug.eprint_s [%sexp (rest'' : Token.t list)]; *)
      let result, _ = parse_expr par_expr in
      (result, rest'')
    | _ -> failwith "invalid syntax"
                                

  let eval_string s = s |> tokenize |> parse2

end

let part_a input =
  let _inputs = [
    "2 * 3 + (4 * 5)";
    "5 + (8 * 3 + 9 + 3 * 4 * 3)";
    "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"; 
    "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2";
  ] in
  let inputs = String.split_lines input in
  let sum = List.fold inputs ~init:0 ~f:(fun acc input -> acc + (Math.tokenize input |> Math.parse2)) in
  print_s [%sexp (sum:int)]

let part_b input =
  let inputs = String.split_lines input in
  let sum = List.fold inputs ~init:0 ~f:(fun acc input ->
      let tokens = Math.tokenize input in
      let result, _ = Math.parse_expr tokens in
      acc + result) in
  print_s [%sexp (sum:int)]

let () =
  let input = In_channel.read_all "input.txt" in
  (* part_a input *)
  part_b input
