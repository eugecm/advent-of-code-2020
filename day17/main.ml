open! Core
open! Stdio

let example = {|.#.
..#
###
|}

module World = struct
  module Coordinates = struct
    module T = struct
      type t = (int * int * int * int) [@@deriving sexp, compare]
    end
    include T
    include Comparable.Make(T)
  end

  type t = Set.M(Coordinates).t [@@deriving sexp_of]

  let of_lines lines : t=
    let active = ref ([]:Coordinates.t list) in
    List.iteri lines ~f:(fun y line ->
        String.to_list line |> List.iteri ~f:(fun x ch ->
            if Char.(=) ch '#' then active := (x, y, 0, 0)::!active else ()));
    Set.of_list (module Coordinates) !active

  let surrounding_coord (x, y, z, w) =
    let surrounding = ref ([]:Coordinates.t list) in
    for x' = x-1 to x+1 do
      for y' = y-1 to y+1 do
        for z' = z-1 to z+1 do
          for w' = w-1 to w+1 do
            if not (x' = x && y' = y && z' = z && w' = w) then 
              surrounding := (x',y',z',w')::!surrounding;
          done
        done
      done
    done;
    !surrounding

  let active_surroundings t c =
    List.filter (surrounding_coord c) ~f:(Set.mem t)

  let evolve t =
    let is_active = Set.mem t in
    Set.fold t ~init:(Set.empty (module Coordinates)) ~f:(fun next active ->
        let surrounding = surrounding_coord active in
        let active_neighbours_count = List.count surrounding ~f:is_active in
        (* Active condition *)
        let next' =
          if (active_neighbours_count = 2) || (active_neighbours_count = 3) then  (
            Set.add next active
          )
          else (
            next
          )
        in
        let inactive_neighbours = List.filter surrounding ~f:(Fn.non is_active) in
        let became_active_neighbours =
          List.fold inactive_neighbours ~init:(Set.empty (module Coordinates)) ~f:(fun acc neighbour ->
              let live_neighbours = active_surroundings t neighbour in
              if Int.(=) (List.length live_neighbours) 3 then (
                Set.add acc neighbour
              )
              else acc)
        in
        Set.union next' became_active_neighbours)
end

let part_a input =
  let m = World.of_lines (String.split_lines input) in
  (* Debug.eprint_s [%sexp (m: World.t)]; *)
  (* Debug.eprint_s [%sexp (World.evolve m: World.t)] *)
  let evolved = Fn.apply_n_times ~n:6 World.evolve m in
  print_s [%sexp ( Set.length evolved : int)]

let _part_b _input =
  ()

let () =
  let input = In_channel.read_all "input.txt" in
  part_a input
(* part_b example *)  
