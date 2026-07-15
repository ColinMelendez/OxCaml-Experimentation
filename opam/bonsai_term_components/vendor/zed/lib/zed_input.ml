(*
 * zed_input.ml
 * ------------
 * Copyright : (c) 2011, Jeremie Dimino <jeremie@dimino.org>
 * Licence   : BSD3
 *
 * This file is a part of Zed, an editor engine.
 *)

module type S = sig
  type event
  type +'a t

  val empty : 'a t
  val add : event list -> 'a -> 'a t -> 'a t
  val remove : event list -> 'a t -> 'a t
  val fold : (event list -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val bindings : 'a t -> (event list * 'a) list

  type 'a resolver
  type 'a pack = Pack : 'b t * ('b -> 'a) -> 'a pack

  val resolver : 'a pack list -> 'a resolver

  type 'a result =
    | Accepted of 'a
    | Continue of 'a resolver
    | Rejected

  val resolve : event -> 'a resolver -> 'a result
end

module Make (Event : Map.OrderedType) = struct
  type event = Event.t

  module Event_map = Map.Make (Event)

  type 'a t = 'a node Event_map.t

  and 'a node =
    | Set of 'a t
    | Val of 'a

  let empty = Event_map.empty

  let rec add events value set =
    match events with
    | [] -> invalid_arg "Zed_input.Make.add"
    | [ event ] -> Event_map.add event (Val value) set
    | event :: events ->
      (match
         try Some (Event_map.find event set) with
         | Not_found -> None
       with
       | None | Some (Val _) -> Event_map.add event (Set (add events value empty)) set
       | Some (Set s) -> Event_map.add event (Set (add events value s)) set)
  ;;

  let rec remove events set =
    match events with
    | [] -> invalid_arg "Zed_input.Make.remove"
    | [ event ] -> Event_map.remove event set
    | event :: events ->
      (match
         try Some (Event_map.find event set) with
         | Not_found -> None
       with
       | None | Some (Val _) -> set
       | Some (Set s) ->
         let s = remove events s in
         if Event_map.is_empty s
         then Event_map.remove event set
         else Event_map.add event (Set s) set)
  ;;

  let fold f set acc =
    let rec loop prefix set acc =
      Event_map.fold
        (fun event node acc ->
          match node with
          | Val v -> f (List.rev (event :: prefix)) v acc
          | Set s -> loop (event :: prefix) s acc)
        set
        acc
    in
    loop [] set acc
  ;;

  let bindings set = List.rev (fold (fun events action l -> (events, action) :: l) set [])

  type 'a pack = Pack : 'b t * ('b -> 'a) -> 'a pack
  type 'a resolver = 'a pack list

  let resolver l = l

  type 'a result =
    | Accepted of 'a
    | Continue of 'a resolver
    | Rejected

  let rec resolve_rec : 'a. event -> 'a pack list -> 'a pack list -> 'a result =
    fun event acc packs ->
    match packs with
    | [] -> if acc = [] then Rejected else Continue (List.rev acc)
    | Pack (set, map) :: packs ->
      (match
         try Some (Event_map.find event set) with
         | Not_found -> None
       with
       | Some (Set set) -> resolve_rec event (Pack (set, map) :: acc) packs
       | Some (Val v) -> Accepted (map v)
       | None -> resolve_rec event acc packs)
  ;;

  let resolve event sets = resolve_rec event [] sets
end
