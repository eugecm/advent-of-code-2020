open! Core
open Stdio

let example = {|0,3,6|}

let rec run spoken last turn until =
  let say_this_turn =
    match Hashtbl.find spoken last with
    | None -> 0
    | Some turns ->
      match turns with
      | [_] -> 0
      | t1::t2::_ -> t1-t2
      | _ -> failwithf "can't happen. last is %d. turn %d" last turn ()
  in
  (* Debug.eprint_s [%sexp (say_this_turn, turn : int * int)]; *)
  Hashtbl.update spoken say_this_turn ~f:(function
      | Some turns -> turn::turns
      | None -> [turn]);
  if turn = until then say_this_turn else run spoken say_this_turn (turn+1) until

let part_a input =
  let init_numbers = String.split input ~on:',' |> List.map ~f:Int.of_string in
  let spoken = Hashtbl.create (module Int) in
  List.iteri init_numbers ~f:(fun i n ->
      Hashtbl.update spoken n ~f:(function
          | Some turns -> (i+1)::turns
          | None -> [i+1]));
  let num = run spoken (List.last_exn init_numbers) (List.length init_numbers + 1) 30000000 in
  print_s [%sexp (num : int)]

(* let part_b input =
 *   () *)

let () =
  let input = {|1,2,16,19,18,0|} in
  part_a input
(* part_b example *)  
