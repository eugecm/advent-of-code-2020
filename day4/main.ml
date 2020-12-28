open! Core
open Stdio
  
let passport_fields = ["byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid"]

let passport_separator = Re2.create_exn {|\n\n|}
let hair_color_re = Re2.create_exn {|^#[0-9a-f]{6}$|}
let eye_color_re = Re2.create_exn {|^amb|blu|brn|gry|grn|hzl|oth$|}
let passport_id_re = Re2.create_exn {|^[0-9]{9}$|}

let load_input () =
  In_channel.read_all "input.txt"

let passport_of_string st =
  String.chop_suffix_if_exists st ~suffix:"\n"
  |> String.split_on_chars ~on:['\n';' ']
  |> List.map ~f:(fun f ->
      let parts = String.split f ~on:':' in
      (List.nth_exn parts 0, List.nth_exn parts 1))

let is_valid_passport p =
  List.for_all passport_fields ~f:(fun field ->
      List.Assoc.find p ~equal:String.equal field |> Option.is_some)

let part_a () =
  let valid_passports = Re2.split passport_separator (load_input ())
  |> List.map ~f:passport_of_string
  |> List.count ~f:is_valid_passport in
  print_s [%sexp (valid_passports : int)]

let is_valid_passport p =
  let open Option in
  let find = List.Assoc.find p ~equal:String.equal in
  let validation = [
    find "byr" >>| Int.of_string >>| Int.between ~low:1920 ~high:2002;
    find "iyr" >>| Int.of_string >>| Int.between ~low:2010 ~high:2020;
    find "eyr" >>| Int.of_string >>| Int.between ~low:2020 ~high:2030;
    find "hgt" >>| (fun height ->
    match String.chop_suffix height ~suffix:"cm" with
    | Some cm -> Int.of_string cm |> Int.between ~low:150 ~high:193
    | None ->
      match String.chop_suffix height ~suffix:"in" with
      | Some inches -> Int.of_string inches |> Int.between ~low:59 ~high:76
      | None -> false);
    find "hcl" >>| Re2.matches hair_color_re;
    find "ecl" >>| Re2.matches eye_color_re;
    find "pid" >>| Re2.matches passport_id_re;
  ]
  in
  List.for_all validation ~f:(Option.value ~default:false)

let part_b () =
  let valid_passports = Re2.split passport_separator (load_input ())
  |> List.map ~f:passport_of_string
  |> List.count ~f:is_valid_passport in
  print_s [%sexp (valid_passports : int)]

let () =
  (* part_a () *)  
  part_b ()
