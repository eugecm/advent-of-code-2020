open! Core
open Stdio

let example = {|sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew|}

module Hexagon = struct
  module T = struct
    type plane =
      | Normal
      | Alternative [@@deriving sexp, compare]

    type t = (int * int * plane) [@@deriving sexp, compare]

    let reference : t = 0, 0, Normal
  end

  include T
  include Comparable.Make(T)

  type direction = | E | SE | SW | W | NW | NE [@@deriving sexp, compare]

  let move (from:t) (dir:direction) =
    match dir with
    | E -> (match from with
        | x, y, Normal -> x+1, y, Normal
        | x, y, Alternative -> x+1, y, Alternative)
    | SE -> (match from with
        | x, y, Normal -> x, y, Alternative
        | x, y, Alternative -> x+1, y-1, Normal)
    | SW -> (match from with
        | x, y, Normal -> x-1, y, Alternative
        | x, y, Alternative -> x, y-1, Normal)
    | W -> (match from with
        | x, y, Normal -> x-1, y, Normal
        | x, y, Alternative -> x-1, y, Alternative)
    | NW -> (match from with
        | x, y, Normal -> x-1, y+1, Alternative
        | x, y, Alternative -> x, y, Normal)
    | NE -> (match from with
        | x, y, Normal -> x, y+1, Alternative
        | x, y, Alternative -> x+1, y, Normal)

  let parse_directions st =
    let cs = String.to_list st in
    let rec parse cs =
      match cs with
      | [] -> []
      | 'e'::rest -> E::(parse rest)
      | 'w'::rest -> W::(parse rest)
      | 's'::'e'::rest -> SE::(parse rest)
      | 's'::'w'::rest -> SW::(parse rest)
      | 'n'::'w'::rest -> NW::(parse rest)
      | 'n'::'e'::rest -> NE::(parse rest)
      | _ -> failwith "invalid characters"
    in
    parse cs

  let adjacent t = List.map [E; SE; SW; W; NW; NE] ~f:(move t)

  let run s =
    Set.fold s ~init:(Set.empty) ~f:(fun next_day black_tile ->
        let adj = adjacent black_tile in
        let n_black_adj = List.count adj ~f:(Set.mem s) in
        let next_black = if Int.equal n_black_adj 0 || Int.(>) n_black_adj 2 then Set.empty else Set.singleton black_tile in
        let next_white = List.fold adj ~init:(Set.empty) ~f:(fun acc tile ->
            let is_black = Set.mem s in
            if is_black tile then acc else begin
              let white_tile = tile in
              let adj = adjacent white_tile in
              if Int.equal (List.count adj ~f:is_black) 2 then
                Set.add acc white_tile
              else acc
            end)
        in
        Set.union_list [next_day; next_black; next_white])
end

let part_a input =
  let directions_group = String.split_lines input |> List.map ~f:Hexagon.parse_directions in
  let flips = List.fold directions_group ~init:(Hexagon.Map.empty) ~f:(fun flips directions ->
      let hexagon = List.fold directions ~init:Hexagon.reference ~f:Hexagon.move in
      Map.update flips hexagon ~f:(function
          | Some n -> n+1
          | None -> 1))
  in
  let answer = Map.count flips ~f:(fun v -> (v % 2) = 1) in
  print_s [%sexp (answer : int)]

let _part_b input =
  let directions_group = String.split_lines input |> List.map ~f:Hexagon.parse_directions in
  let flips = List.fold directions_group ~init:(Hexagon.Map.empty) ~f:(fun flips directions ->
      let hexagon = List.fold directions ~init:Hexagon.reference ~f:Hexagon.move in
      Map.update flips hexagon ~f:(function
          | Some n -> n+1
          | None -> 1))
  in
  let initial_state = Map.filter flips ~f:(fun v -> (v % 2) = 1) |> Map.key_set in
  let final_state = Fn.apply_n_times ~n:100 Hexagon.run initial_state in
  print_s [%sexp (Set.length final_state : int)]

let () =
  let _input = In_channel.read_all "input.txt" in
  (* part_a input *)
  _part_b _input
