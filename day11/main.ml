open! Core
open Stdio

let example = {|L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL
|}

module Map = struct
  type location =
    | Floor
    | Occupied
    | Empty
  [@@deriving sexp, compare, equal]

  type t = {
    height: int;
    width: int;
    seats: location array}
  [@@deriving sexp, compare, equal]

  let get t ~x ~y =
    let row = t.width * y in
    Array.get t.seats (row + x)

  let i_to_coord t ~i =
    (i%t.width, i/t.width)

  let of_lines lines =
    let height = List.length lines in
    let width = List.nth_exn lines 0 |> String.length in
    let seats =
      String.concat lines
      |> String.to_list
      |> List.map ~f:(function
          | 'L' -> Empty
          | '#' -> Occupied
          | '.' -> Floor
          | _ -> failwith "invalid position")
      |> Array.of_list
    in
    {height;width; seats}

  let surroundings t ~x ~y =
    let from_x = if x = 0 then 0 else x-1 in
    let to_x = if x = t.width -1 then x else x+1 in
    let from_y = if y = 0 then 0 else y-1 in
    let to_y = if y = t.height -1 then y else y+1 in
    let surroundings' = ref [] in
    for xi = from_x to to_x do
      for yi = from_y to to_y do
        if xi <> x || yi <> y then
          surroundings' := (get t ~x:xi ~y:yi) :: !surroundings';
      done
    done;
    !surroundings'
      
  let rec first_visible t ~x ~y (dir_x, dir_y) =
    let (x', y') = (x+dir_x), (y+dir_y) in
    if not (Int.between x' ~low:0 ~high:(t.width-1)) || not (Int.between y' ~low:0 ~high:(t.height-1)) then
      None
    else
      let next = get t ~x:x' ~y:y' in
      match next with
      | Floor -> first_visible t ~x:x' ~y:y' (dir_x, dir_y)
      | seat -> Some seat

  let can_see t ~x ~y =
    let directions = [
      0, 1;
      1, 1;
      1, 0;
      1, -1;
      0, -1;
      -1, -1;
      -1, 0;
      -1, 1;]
    in
    List.filter_map directions ~f:(fun dir -> first_visible t ~x ~y dir)

  let evolve_position t ~x ~y =
    let sur = surroundings t ~x ~y in
    let position = get t ~x ~y in
    match position with
    | Empty -> if List.count sur ~f:(equal_location Occupied) = 0 then Occupied else Empty
    | Occupied -> if List.count sur ~f:(equal_location Occupied) >= 4 then Empty else Occupied
    | s -> s

  let evolve_position2 t ~x ~y =
    let sur = can_see t ~x ~y in
    let position = get t ~x ~y in
    match position with
    | Empty -> if List.count sur ~f:(equal_location Occupied) = 0 then Occupied else Empty
    | Occupied -> if List.count sur ~f:(equal_location Occupied) >= 5 then Empty else Occupied
    | s -> s

  let evolve t =
    let seats = Array.mapi t.seats ~f:(fun i _ ->
        let (x, y) = i_to_coord t ~i in
        evolve_position t ~x ~y)
    in
    {t with seats}

  let evolve2 t =
    let seats = Array.mapi t.seats ~f:(fun i _ ->
        let (x, y) = i_to_coord t ~i in
        evolve_position2 t ~x ~y)
    in
    {t with seats}

end

let rec evolve_until_equal t =
  let e = (Map.evolve t) in
  if Map.equal e t then e else evolve_until_equal e

let rec evolve2_until_equal t =
  let e = (Map.evolve2 t) in
  if Map.equal e t then e else evolve2_until_equal e


let part_a input =
  let map = String.split_lines input |> Map.of_lines in
  let stable = evolve_until_equal map in
  let occupied_seats = Array.count stable.seats ~f:(Map.equal_location Occupied) in
  print_s [%sexp (occupied_seats : int)]

let part_b input =
  let map = String.split_lines input |> Map.of_lines in
  let stable = evolve2_until_equal map in
  let occupied_seats = Array.count stable.seats ~f:(Map.equal_location Occupied) in
  print_s [%sexp (occupied_seats : int)]

let () =
  let input = In_channel.read_all "input.txt" in
  (* part_a input *)
  part_b input
