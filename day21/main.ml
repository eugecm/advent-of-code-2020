open! Core
open Stdio

let example = {|mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)|}

let input_re = Re2.create_exn {|^([a-z ]+) \(contains ([a-z, ]+)\)$|}

module Ingredient : sig
  type t
  val of_string : string -> t
  include Sexpable.S with type t := t
  include Comparable.S with type t := t
end = struct
  module T = struct
    type t = string [@@deriving sexp, compare]
  end
  include T
  include Comparable.Make(T)

  let of_string a = a
end

module Allergen : sig
  type t
  val of_string : string -> t
  include Sexpable.S with type t := t
  include Comparable.S with type t := t
end = struct
  module T = struct
    type t = string [@@deriving sexp, compare]
  end
  include T
  include Comparable.Make(T)
  let of_string a = a
end

(*
mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)

Dont have:
- kfcds
- nhms
- sbzzf
- trh

mxmxvkd sqjhc (contains dairy, fish)
fvjkl mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd (contains fish)

A B C D [1,2]
E F G A [1]
C F     [3]
C A G   [2]

E = 1?

Impossible. A B C or D must be 1
*)


module Constraint = struct
  (* The sum of allergents for every ingredient must be a superset of the allergens *)
  type t = (Set.M(Ingredient).t * Set.M(Allergen).t) [@@deriving sexp]

  let of_string s : t=
    let matches = Re2.find_submatches_exn input_re s in
    let ingredients = Array.get matches 1
                      |> Option.value_exn
                      |> String.split ~on:' '
                      |> List.map ~f:(Ingredient.of_string)
                      |> Set.of_list (module Ingredient)
    in
    let allergens = Array.get matches 2
                    |> Option.value_exn
                    |> String.split ~on:','
                    |> List.map ~f:(String.chop_prefix_if_exists ~prefix:" ")
                    |> List.map ~f:(Allergen.of_string)
                    |> Set.of_list (module Allergen)
    in
    (ingredients, allergens)

  let allergen_candidates_for_ingredient (ts: t list) i : Set.M(Allergen).t =
    List.fold ts ~init:(Set.empty (module Allergen)) ~f:(fun acc (ingredients, allergens) ->
        if Set.mem ingredients i then Set.union acc allergens else acc)

  let can_contain_allergen (ts: t list) i a =
    List.for_all ts ~f:(fun (ingredients, allergens) ->
        if Set.mem allergens a then Set.mem ingredients i else true)

  let find_allergens (ts: t list) i =
    let allergen_candidates = allergen_candidates_for_ingredient ts i in
    Set.filter allergen_candidates ~f:(fun allergen -> can_contain_allergen ts i allergen)

  let is_valid ts solution =
    List.for_all solution ~f:(fun (ingredient, allergen) ->
        List.for_all solution ~f:(fun (i, a) -> Ingredient.equal i ingredient || not @@ Allergen.equal a allergen)
        &&
        can_contain_allergen ts ingredient allergen
      )

  let rec find_solution (ts : t list) choices (solution: (Ingredient.t * Allergen.t) list) : (Ingredient.t * Allergen.t) list option =
    if not @@ is_valid ts solution then None
      else if List.is_empty choices then Some solution
    else
      List.find_map choices ~f:(fun (ingredient, allergen) ->
          let next_choices = List.filter choices ~f:(fun (i, _) -> Ingredient.(<>) i ingredient) in
          find_solution ts next_choices ((ingredient, allergen)::solution))

end

let part_a input =
  let constraints = String.split_lines input
                    |> List.map ~f:Constraint.of_string
  in
  let all_ingredients = List.fold constraints ~init:(Set.empty (module Ingredient)) ~f:(fun acc (ingredients, _) ->
      Set.union acc ingredients)
  in
  let allergen_candidates_for_ingredients = Set.to_list all_ingredients |> List.map ~f:(fun i ->
      (i, Constraint.allergen_candidates_for_ingredient constraints i)) in
  let cant_have_allergens = List.filter_map allergen_candidates_for_ingredients ~f:(fun (ingredient, candidate_allergens) ->
      if Set.for_all candidate_allergens ~f:(fun a -> not @@ Constraint.can_contain_allergen constraints ingredient a) then
        Some ingredient else None
    ) |> Set.of_list (module Ingredient)
  in
  let appearances = List.sum (module Int) constraints ~f:(fun (ingredients, _) ->
      Set.length (Set.inter ingredients cant_have_allergens))
  in
  print_s [%sexp (appearances : int)]
  (* List.iter cant_have_allergens ~f:(fun ingredient -> print_s [%message "cant have allergen" (ingredient : Ingredient.t)]) *)

let _part_b _input =
  let constraints = String.split_lines _input
                    |> List.map ~f:Constraint.of_string
  in
  let all_ingredients = List.fold constraints ~init:(Set.empty (module Ingredient)) ~f:(fun acc (ingredients, _) ->
      Set.union acc ingredients)
  in
  let allergen_candidates_for_ingredients = Set.to_list all_ingredients |> List.map ~f:(fun i ->
      (i, Constraint.allergen_candidates_for_ingredient constraints i)) in
  let cant_have_allergens = List.filter_map allergen_candidates_for_ingredients ~f:(fun (ingredient, candidate_allergens) ->
      if Set.for_all candidate_allergens ~f:(fun a -> not @@ Constraint.can_contain_allergen constraints ingredient a) then
        Some ingredient else None
    ) |> Set.of_list (module Ingredient)
  in
  let simpler_constraints = List.map constraints ~f:(fun (ingredients, allergens) ->
      (Set.diff ingredients cant_have_allergens, allergens))
  in
  let may_have_allergens = Set.diff all_ingredients cant_have_allergens in
  let choices = Set.to_list may_have_allergens
                |> List.concat_map ~f:(fun ingredient ->
                    let candidates = Constraint.find_allergens simpler_constraints ingredient |> Set.to_list in
                    List.map candidates ~f:(fun c -> (ingredient, c)))
  in
  let solution = Constraint.find_solution simpler_constraints choices [] in
  print_s [%sexp (solution: (Ingredient.t * Allergen.t) list option)]

let () =
  let _input = In_channel.read_all "input.txt" in
  (* part_a _input *)
  _part_b _input

(*
(xgtj (dairy))
(ztdctgq (eggs))
(bdnrnx (fish))
(cdvjp (nuts))
(jdggtft (peanuts))
(mdbq (sesame))
(rmd (shellfish))
(lgllb (soy))

   xgtj,ztdctgq,bdnrnx,cdvjp,jdggtft,mdbq,rmd,lgllb

*)
