open! Core
open Stdio

type cell =
  | Tree
  | Empty
[@@deriving sexp, compare]

module Map = struct
  type t = {
    height: int;
    width: int;
    cells: cell array} [@@deriving sexp, compare]

  let get t ~x ~y =
    (* Support out of bounds to the right and down*)
    let x_norm = x % t.width in
    let y_norm = y % t.height in
    let row = t.width * y_norm in
    Array.get t.cells (row + x_norm)

  let of_lines lines =
    let height = List.length lines in
    let width = List.nth_exn lines 0 |> String.length in
    let cells =
      String.concat lines
      |> String.to_list
      |> List.map ~f:(function
          | '.' -> Empty
          | '#' -> Tree
          | _ -> failwith "Unknown cell")
      |> Array.of_list
    in
    {height;width; cells}

end

let part_a () =
  let lines = In_channel.read_lines "input.txt" in
  let m = Map.of_lines lines in
  let coords = List.init m.height ~f:(fun i -> (i, i*3)) in
  let tree_count = List.count coords ~f:(fun (y, x) -> (compare_cell (Map.get m ~x ~y) Tree) = 0) in
  print_s [%sexp (tree_count : int)]
  

let part_b () =
  let lines = In_channel.read_lines "input.txt" in
  let m = Map.of_lines lines in
  let coord_fs = [
    (fun i -> (i, i));
    (fun i -> (i, i*3));
    (fun i -> (i, i*5));
    (fun i -> (i, i*7));
    (fun i -> (i*2, i));] in
  let coord_sets = List.map coord_fs ~f:(fun f ->
      Sequence.unfold ~init:0 ~f:(fun i ->
          let (y, x) = f i in
          if y > (m.height-1) then None else Some ((y, x), (i+1)))
      |> Sequence.to_list) in
  let tree_counts = List.map coord_sets ~f:(fun coords ->
      List.count coords ~f:(fun (y, x) -> (compare_cell (Map.get m ~x ~y) Tree) = 0)) in
  print_s [%sexp (tree_counts : int list)];
  let result = List.fold ~init:1 ~f:Int.( * ) tree_counts in
  print_s [%sexp (result : int)]

let () =
  (* part_a () *)
  part_b ()
