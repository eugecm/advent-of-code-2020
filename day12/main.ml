open! Core
open Stdio

let example = {|F10
N3
F7
R90
F11|}

let instruction_pattern = Re2.create_exn {|(N|S|E|W|L|R|F)(\d+)|}

type action =
  | North of int
  | South of int
  | East of int
  | West of int
  | Left of int
  | Right of int
  | Forward of int
[@@deriving sexp, equal]

type direction =
  | N
  | S
  | E
  | W
[@@deriving sexp, equal]

let action_of_string s =
  let matches = Re2.find_submatches_exn instruction_pattern s in
  let d = Array.get matches 1 |> Option.value_exn in
  let n = Array.get matches 2 |> Option.value_exn |> Int.of_string in
  match d with
  | "N" -> North n
  | "S" -> South n
  | "E" ->  East n
  | "W" -> West n
  | "L" -> Left n
  | "R" -> Right n
  | "F" -> Forward n
  | _ -> failwithf "unknown instruction %s" d ()

let l90 dir =
  match dir with
  | N -> W
  | W -> S
  | S -> E
  | E -> N

let r90 dir =
  match dir with
  | N -> E
  | E -> S
  | S -> W
  | W -> N

let move (x,y,dir) action =
  match action with
  | North n -> (x, y+n, dir)
  | South n -> (x, y-n, dir)
  | East n -> (x+n, y, dir)
  | West n -> (x-n, y, dir)
  | Left n -> begin
      match n with
      | 90 -> (x, y, l90(dir))
      | 180 -> (x, y, l90(l90(dir)))
      | 270 -> (x, y, l90(l90(l90(dir))))
      | _ -> failwithf "unexpected left degrees %d" n ()
    end
  | Right n -> begin
      match n with
      | 90 -> (x, y, r90(dir))
      | 180 -> (x, y, r90(r90(dir)))
      | 270 -> (x, y, r90(r90(r90(dir))))
      | _ -> failwithf "unexpected right degrees %d" n ()
    end
  | Forward n -> begin
      match dir with
      | N -> (x, y+n, dir)
      | S-> (x, y-n, dir)
      | E-> (x+n, y, dir)
      | W-> (x-n, y, dir)
    end

let part_a input =
  let actions = String.split_lines input |> List.map ~f:action_of_string in
  let (x, y, _) = List.fold actions ~init:(0,0,E) ~f:move in
  let manhattan = (Int.abs x) + (Int.abs y) in
  print_s [%sexp (manhattan : int)]

let r90 (x,y) = (y, -x)
let l90 (x,y) = (-y, x)

let move (x, y) (x_w, y_w) action =
  match action with
  | North n -> (x, y), (x_w, y_w+n)
  | South n -> (x, y), (x_w, y_w-n)
  | East n -> (x, y), (x_w+n, y_w)
  | West n -> (x, y), (x_w-n, y_w)
  | Left n -> begin
      match n with
      | 90 -> (x,y), l90(x_w, y_w)
      | 180 -> (x,y), l90(l90(x_w, y_w))
      | 270 -> (x,y), l90(l90(l90(x_w, y_w)))
      | _ -> failwithf "unexpected left degrees %d" n ()
    end
  | Right n -> begin
      match n with
      | 90 -> (x,y), r90(x_w, y_w)
      | 180 -> (x,y), r90(r90(x_w, y_w))
      | 270 -> (x,y), r90(r90(r90(x_w, y_w)))
      | _ -> failwithf "unexpected right degrees %d" n ()
    end
  | Forward n -> (x + n*x_w, y + n*y_w), (x_w, y_w)

let part_b input =
  let actions = String.split_lines input |> List.map ~f:action_of_string in
  let ((x, y), _)  = List.fold actions ~init:((0,0), (10, 1)) ~f:(fun (ship, waypoint) action ->
      move ship waypoint action) in
  let manhattan = (Int.abs x) + (Int.abs y) in
  print_s [%sexp (manhattan : int)]

let () =
  let input = In_channel.read_all "input.txt" in
  (* part_a input *)
  part_b input
