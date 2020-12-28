open! Core
open Stdio

let example = {|Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10|}

let example2 = {|Player 1:
43
19

Player 2:
2
29
14|}

let to_string q q'=
  String.concat ~sep:"$" [
    (Sexp.to_string [%sexp (q : int Fqueue.t)]);
    (Sexp.to_string [%sexp (q' : int Fqueue.t)]);
  ]

let cache_hit_counter = ref 0

let copy_n q n =
  let q_list = Fqueue.to_list q in
  List.take q_list n |> Fqueue.of_list

let rec play_game p1 p2 =
  (* Debug.eprint_s [%message "New game" (p1 : int Fqueue.t) (p2: int Fqueue.t)]; *)
  let previous_rounds = Hash_set.create (module String) in
  let rec play_round p1 p2 =
    (* Debug.eprint_s [%message "Round" (p1 : int Fqueue.t) (p2: int Fqueue.t)]; *)
    let state = to_string p1 p2 in
    (* Debug.eprint_s [%sexp (state : string)]; *)
    if Hash_set.mem previous_rounds state then (
      p1, p2, true
    )
    else (
      Hash_set.add previous_rounds state;
      if Fqueue.is_empty p1 || Fqueue.is_empty p2 then
        p1, p2, false
      else
        let c1, p1' = Fqueue.dequeue_exn p1 in
        let c2, p2' = Fqueue.dequeue_exn p2 in
        let winner = if (c1 <= Fqueue.length p1') && (c2 <= Fqueue.length p2') then (
            let _, _, winner = play_game (copy_n p1' c1) (copy_n p2' c2) in
            winner) else (
            if c1 > c2 then
              1
            else
              2
          ) in
        match winner with
        | 1 -> 
          let p1 = Fqueue.enqueue (Fqueue.enqueue p1' c1) c2 in
          play_round p1 p2'
        | 2 -> 
          let p2 = Fqueue.enqueue (Fqueue.enqueue p2' c2) c1 in
          play_round p1' p2
        | _ -> failwith "invalid winner"
    )
  in
  let p1', p2', early_exit = play_round p1 p2 in
  if early_exit then p1', p2', 1 else p1', p2', (if Fqueue.is_empty p1' then 2 else 1)

let part_b input =
  let groups = String.split_lines input |> List.group ~break:(fun _ b -> String.is_empty b) in
  let player1 = List.nth_exn groups 0 |> List.tl_exn |> List.map ~f:Int.of_string in
  let player2 = List.nth_exn groups 1 |> List.tl_exn |> List.tl_exn |> List.map ~f:Int.of_string in
  let p1, p2, winner_i = play_game (Fqueue.of_list player1) (Fqueue.of_list player2) in
  let winner = if winner_i = 1 then p1 else p2 in
  let score = Fqueue.to_list winner |> List.rev |> List.foldi ~init:0 ~f:(fun i acc card -> acc + ((i+1) * card)) in
  print_s [%sexp (score : int)]

let () =
  let _input = In_channel.read_all "input.txt" in
  part_b _input
