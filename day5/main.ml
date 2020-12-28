open! Core
open Stdio

let take_upper_half (a, b) = (((a + b) / 2) + 1, b)

let take_lower_half (a, b) = (a, (a + b) / 2)

let parse_boarding_pass s =
  let (_, r), (_,c) = String.fold s ~init:((0, 127), (0, 7)) ~f:(fun (row, col) c ->
      match c with
      | 'F' -> (take_lower_half row, col)
      | 'B' -> (take_upper_half row, col)
      | 'R' -> (row, take_upper_half col)
      | 'L' -> (row, take_lower_half col)
      | _ -> failwith "invalid step"
    ) in
  (r, c, r * 8 + c)

let read_input () = In_channel.read_lines "input.txt"

let part_a () =
  let (_, _, max_id) = read_input ()
                       |> List.map ~f:parse_boarding_pass
                       |> List.max_elt ~compare:(fun (_,_,id1) (_,_,id2) -> Int.compare id1 id2)
                       |> Option.value ~default:(0, 0, 0)
  in
  print_s [%sexp (max_id: int)]

let part_b () =
  let boarding_pass_ids =
    read_input ()
    |> List.map ~f:(fun s ->
        let (_, _, id) = parse_boarding_pass s in
        id)
    |> List.sort ~compare:Int.compare
    |> Array.of_list
  in
  let min_pass = Array.min_elt ~compare:Int.compare boarding_pass_ids |> Option.value ~default:0 in
  let missing = Array.find_mapi boarding_pass_ids ~f:(fun i id ->
      if (i+min_pass) <> id then Some (i+min_pass) else None)
                |> Option.value ~default:0
  in
  print_s [%sexp (missing : int)]

let () =
  (* part_a () *)
  part_b () 
