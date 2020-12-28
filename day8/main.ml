open! Core
open Stdio

module Cpu = struct
  module Instruction = struct
    type t =
      | Acc of int
      | Jmp of int
      | Nop of int
    [@@deriving sexp_of]

    let parse_program s =
      let lines = String.split_lines s in
      Array.of_list_map lines ~f:(fun line ->
          match String.split line ~on:' ' with
          | "acc"::n::[] -> Acc (Int.of_string n)
          | "jmp"::n::[] -> Jmp (Int.of_string n)
          | "nop"::n::[] -> Nop (Int.of_string n)
          | _ -> failwith "Invalid program")
  end

  type t = {
    accumulator: int;
    ip: int;
    last_ip: int;
    code: Instruction.t array;
  } [@@deriving sexp_of]

  type state =
    | Running of t
    | Stopped of t
  [@@deriving sexp_of]

  let create program =
    {accumulator=0; ip=0; code=program; last_ip=0}

  let execute t =
    if t.ip = Array.length t.code then Stopped t else
    match t.code.(t.ip) with
    | Acc n -> Running {t with accumulator=(t.accumulator + n); ip=(t.ip + 1); last_ip=t.ip}
    | Jmp n -> Running {t with ip=(t.ip + n); last_ip=t.ip}
    | Nop _ -> Running {t with ip=(t.ip + 1); last_ip=t.ip}

  let rec run_steps t ~n =
    if n = 0 then t else
      match execute t with
      | Running cpu' -> run_steps cpu' ~n:(n-1)
      | Stopped cpu' -> cpu'

  let rec run_until_done t =
    match execute t with
    | Running cpu -> run_until_done cpu
    | Stopped cpu -> cpu
end

let rec run_until_repeat (cpu:Cpu.t) ~seen =
  if Set.mem seen cpu.ip then Cpu.Running cpu else
    match Cpu.execute cpu with
    | Running cpu' -> run_until_repeat cpu' ~seen:(Set.add seen cpu.ip)
    | Stopped cpu' -> Cpu.Stopped cpu'

let part_a input =
  Cpu.Instruction.parse_program input
  |> Cpu.create
  |> run_until_repeat ~seen:(Int.Set.empty)
  |> [%sexp_of: Cpu.state]
  |> print_s

let part_b input =
  let instructions = Cpu.Instruction.parse_program input in
  let swap_nop_jmp instruction =
    match instruction with
    | Cpu.Instruction.Nop n -> Cpu.Instruction.Jmp n
    | Cpu.Instruction.Jmp n -> Cpu.Instruction.Nop n
    | a -> a
  in
  let rec swap_until_no_repeat i =
    if i = Array.length instructions then failwith "could not find bad instruction" else (
      Array.set instructions i (swap_nop_jmp instructions.(i));
      let cpu = Cpu.create instructions in
      match run_until_repeat cpu ~seen:(Int.Set.empty) with
      | Stopped cpu -> cpu.accumulator
      | Running _ ->
          Array.set instructions i (swap_nop_jmp instructions.(i));
          swap_until_no_repeat (i+1)
    )
  in
  swap_until_no_repeat 0 |> [%sexp_of: int] |> print_s

let () =
  let input = In_channel.read_all "input.txt" in
  (* part_a input *)
  part_b input
