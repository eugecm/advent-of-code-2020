open! Core
open Stdio

let example = {|mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1|}

let mask_pattern = Re2.create_exn {|^mask = ([X01]{36})$|}
let mem_write_pattern = Re2.create_exn {|^mem\[(\d+)\] = (\d+)$|}

module Input = struct
  type value = Value of int [@@deriving sexp, equal]
  type address = Address of int [@@deriving sexp, equal]
  type mask = {
    and_: int;
    or_: int;
  } [@@deriving sexp, equal]

  type t =
    | Mask of mask
    | Mem_write of address * value
  [@@deriving sexp, equal]

  let parse input =
    String.split_lines input
    |> List.map ~f:(fun line ->
        if Re2.matches mask_pattern line then
          let matches = Re2.find_submatches_exn mask_pattern line in
          let mask_str = Array.get matches 1 |> Option.value_exn in
          let mask_and = "0b" ^ String.substr_replace_all mask_str ~pattern:"X" ~with_:"1" |> int_of_string in
          let mask_or = "0b" ^ String.substr_replace_all mask_str ~pattern:"X" ~with_:"0" |> int_of_string in
          Mask {and_=mask_and; or_=mask_or}
        else if Re2.matches mem_write_pattern line then
          let matches = Re2.find_submatches_exn mem_write_pattern line in
          let address = Array.get matches 1 |> Option.value_exn |> Int.of_string in
          let value = Array.get matches 2 |> Option.value_exn |> Int.of_string in
          Mem_write (Address address, Value value)
        else
          failwith "invalid input")
end

module Input2 = struct
  type value = Value of int [@@deriving sexp, equal]
  type address = Address of int [@@deriving sexp, equal]
  type t =
    | Mask of string
    | Mem_write of address * value
  [@@deriving sexp, equal]

  let parse input =
    String.split_lines input
    |> List.map ~f:(fun line ->
        if Re2.matches mask_pattern line then
          let matches = Re2.find_submatches_exn mask_pattern line in
          let mask_str = Array.get matches 1 |> Option.value_exn in
          Mask mask_str
        else if Re2.matches mem_write_pattern line then
          let matches = Re2.find_submatches_exn mem_write_pattern line in
          let address = Array.get matches 1 |> Option.value_exn |> Int.of_string in
          let value = Array.get matches 2 |> Option.value_exn |> Int.of_string in
          Mem_write (Address address, Value value)
        else
          failwith "invalid input")
end

let rec int_to_bits = function
  | 0 -> "0"
  | 1 -> "1"
  | n -> (int_to_bits (Int.shift_right n 1)) ^ (int_to_bits (n%2))

let apply_mask m n =
  let n_bin = int_to_bits n in
  let m_len = String.length m in
  let prefix = String.make (m_len - String.length n_bin) '0' in
  let n_bin = prefix ^ n_bin in
  let masked = String.mapi n_bin ~f:(fun i c ->
      match String.get m i with
      | '0' -> c
      | '1' -> '1'
      | 'X' -> 'X'
      | o -> failwithf "unknown %c" o ())
  in
  String.fold masked ~init:[Fqueue.empty] ~f:(fun combinations c ->
      if Char.(=) c 'X' then
        List.append
          (List.map combinations ~f:(fun q -> Fqueue.enqueue q '1'))
          (List.map combinations ~f:(fun q -> Fqueue.enqueue q '0'))
      else
        List.map combinations ~f:(fun q -> Fqueue.enqueue q c))
  |> List.map ~f:(fun q -> Fqueue.to_list q |> String.of_char_list |> String.(^) "0b" |> int_of_string)

let run (instructions:Input.t list) =
  let acc = (Map.empty (module Int), ({and_=0;or_=0} : Input.mask)) in
  let mem, _ = List.fold instructions ~init:acc ~f:(fun (mem, m) instruction ->
      match instruction with
      | Mask m ->
        (mem, m)
      | Mem_write (Address addr, Value value) ->
        let value' = Int.(land) m.and_ value in
        let value' = Int.(lor) m.or_ value' in
        (Map.update mem addr ~f:(function _ -> value'), m))
  in
  Map.fold mem ~init:0 ~f:(fun ~key:_ ~data acc -> acc + data)

let run2 (instructions:Input2.t list) =
  let acc = (Map.empty (module Int), "") in
  let mem, _ = List.fold instructions ~init:acc ~f:(fun (mem, m) instruction ->
      match instruction with
      | Mask m ->
        (mem, m)
      | Mem_write (Address addr, Value value) ->
        let addresses = apply_mask m addr in
        let mem' = List.fold addresses ~init:mem ~f:(fun mem addr ->
            Map.update mem addr ~f:(function _ -> value))
        in
        (mem', m))
  in
  Map.fold mem ~init:0 ~f:(fun ~key:_ ~data acc -> acc + data)


let part_a input =
  let instructions = Input.parse input in
  let output = run instructions in
  print_s [%sexp (output : int)]

let part_b input =
  let instructions = Input2.parse input in
  let output = run2 instructions in
  print_s [%sexp (output : int)]

let () =
  let input = In_channel.read_all "input.txt" in
  (* part_a input *)
  part_b input
