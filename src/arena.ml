(*
    Copyright (C) 2004  Justin Koser <justin@amoebaville.net>

    This file is part of Frobots.

    Frobots is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    Frobots is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with Frobots; if not, write to the Free Software
    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)
open Printf;;
open Logging;;

type wall = (float * float) * (float * float);;

let wall x0 y0 x1 y1 = (x0, y0), (x1, y1);;


module Geometry = struct

  type point = float * float

  let dist (x0, y0) (x1, y1) =
    let dx = x0 -. x1 in
    let dy = y0 -. y1 in
    sqrt (dx *. dx +. dy *. dy)
  
  type segment = point * point

  let pointy_segment ((x0, y0), (x1, y1): segment): bool =
    x0 = x1 && y0 = y1

  (* Here, a line is represented as a normal unit vector \lbrack a; b\rbrack, a
     constant c such that $ax+by+c=0$, and a distance p from the origin.  Since
     this is lossy, we keep the endpoints around in the tree below. *)
  type nor_line = float * float * float * float

  let nor_line ((x0, y0), (x1, y1): segment): nor_line =
    let a = y0 -. y1
    and b = x1 -. x0 in
    let norm = sqrt (a *. a +. b *. b) in
    let a = a /. norm
    and b = b /. norm in
    let c = -. a *. x0 -. b *. y0 in
    let big_d = c /. sqrt (a *. a +. b *. b) in
    log "nor_line" 10
      (sprintf "(a = %f, b = %f, c = %f, big_d = %f)" a b c big_d);
    (a, b, c, big_d)

  let crosses ((x0, y0), (x1, y1): segment) (a, b, _, big_d: nor_line): bool =
    (* project both points onto line's normal *)
    (x0 *. a +. y0 *. b +. big_d) *. (x1 *. a +. y1 *. b +. big_d) <= 0.0

  let segments_intersect (s1: segment) (s2: segment): bool =
    assert ((not (pointy_segment s1)) && (not (pointy_segment s2)));
    crosses s1 (nor_line s2) && crosses s2 (nor_line s1)

  let close_to_line ((x, y): point) (a, b, _, big_d: nor_line)
                    (r: float) : bool =
    assert (r >= 0.0);
    (* project onto line's normal *)
    let d = abs_float (x *. a +. y *. b +. big_d) in
    log "close_to_line" 10
      (sprintf "(%f, %f) (a = %.1f, b = %.1f, _, big_d = %.1f) -> %f -> %b"
       x y a b big_d d (d <= r));
    d <= r

  let close_to_segment ((x, y): point) ((x0, y0), (x1, y1) as s: segment)
                       (r: float): bool =
    let a, b, _, big_d as l = nor_line s in
    (close_to_line (x, y) l r &&
     let xm = x0 +. x1 /. 2.0
     and ym = y0 +. y1 /. 2.0 in
     close_to_line (x, y) (nor_line ((xm, ym), (xm +. a, ym +. b)))
                   (dist (x0, y0) (xm, ym))) ||
    dist (x, y) (x1, y1) <= r ||
    dist (x, y) (x1, y1) <= r
end


module Quadtree = struct

  type node = Leaf
            | Inner of t list

  and t = {node: node; x: float; size_x: float;
           y: float; size_y: float; walls: wall list}

  (* TODO: build a non-trivial tree *)
  let make (size_x: float) (size_y: float) (ws: wall list): t =
    {node = Leaf; x = 0.0; size_x = size_x;
     y = 0.0; size_y = size_y; walls = ws}

  (* Return [None] if no collision, or [Some theta] if a collision did occur
     with a line whose normal is [theta] radians from the x-axis. *)
  let collides (x0: float) (y0: float) (x1: float) (y1: float)
               (r: float) (qtree: t): float option =
    let path = (x0, y0), (x1, y1) in
    let (a, b, c, p) = Geometry.nor_line path in
    let rec collides' qtree =
      let {node=node; x=x; size_x=size_x;
           y=y; size_y=size_y; walls=walls} = qtree in
      let f acc wall =
        if acc <> None then acc else
        if Geometry.segments_intersect path wall ||
           Geometry.close_to_segment (x1, y1) wall r then
          Some (atan2 b a)
        else
          None
      in
      let res = List.fold_left f None walls in
      if res <> None then res else
      match node with
      | Leaf -> None
      | Inner ts -> assert false
          (* TODO: decide where to recurse *)
          (* Case 1: line passes through sub-box *)
          (* Case 2: endpoint is within r of sub-box *)
    in
    collides' qtree
end


module type S = sig
  type obj

  class arena :
    x:float -> y:float -> walls:wall list -> unit ->
    object
      method add_object :
        obj -> x:float -> y:float -> r:float ->
               ?v:float * float ->
               ?a:float -> ?omega:float ->
               unit -> unit
      method iter :
        (obj -> x:float -> y:float -> r:float ->
                v:float * float ->
                a:float -> omega:float ->
                unit) -> unit
      method modify_object :
        obj -> ?x:float -> ?y:float ->
               ?v:float * float ->
               ?a:float -> ?omega:float ->
               unit -> unit
      method remove_object : obj -> unit
      method step_all : collision:(obj -> obj -> bool) -> unit
    end
end


module Make (O: sig type t val compare : t -> t -> int end) = struct
  type obj = O.t

  module ObjMap = Map.Make (O)
  type obj_info = {mutable x:float; mutable y:float; mutable r:float;
                   mutable v:float * float; mutable a:float;
                   mutable omega:float}
  type obj_store = obj_info ObjMap.t

  class arena ~x:(x: float) ~y:(y: float) ~walls:(ws: wall list) () =
    object
      val qtree : Quadtree.t = Quadtree.make x y ws
      val mutable objs : obj_store = ObjMap.empty

      method add_object (obj: O.t) ~x:(x: float) ~y:(y: float) ~r:(r: float)
                        ?(v = (0.0, 0.0)) ?(a = 0.0) ?(omega = 0.0) () =
        assert (r >= 0.0);
        objs <- ObjMap.add obj {x=x; y=y; r=r; v=v; a=a; omega=omega} objs

      method iter (f: O.t -> x:float -> y:float -> r:float ->
                      v:float * float ->
                      a:float -> omega:float ->
                      unit) =
        ObjMap.iter (fun obj {x=x; y=y; r=r; v=v; a=a; omega=omega} ->
                      f obj ~x:x ~y:y ~r:r ~v:v ~a:a ~omega:omega) objs

      method modify_object (obj: O.t)
                           ?x:(xo: float option) ?y:(yo: float option)
                           ?v:(vo: (float * float) option)
                           ?a:(ao: float option) ?omega:(wo: float option) () =
        let info : obj_info = ObjMap.find obj objs in
        Util.if_some xo (fun x -> info.x <- x);
        Util.if_some yo (fun y -> info.y <- y);
        Util.if_some vo (fun v -> info.v <- v);
        Util.if_some ao (fun a -> info.a <- a);
        Util.if_some wo (fun w -> info.omega <- w);

      method remove_object (obj: O.t) =
        objs <- ObjMap.remove obj objs

      method step_all ~collision:(coll: O.t -> O.t -> bool) =
        let step_obj obj info =
          (* Since collisions may have destroyed this object,
             we must check for it in the map. *)
          if ObjMap.mem obj objs then begin
            let info = ObjMap.find obj objs in
            let v_r, v_theta = info.v in
            (* A poor, but cheap to calculate version of physics. *)
            let v_theta = v_theta +. info.omega in
            let d = v_r +. info.a *. info.a /. 2.0 in
            let v_r = v_r +. info.a in
            info.v <- v_r, v_theta;
            (* If there's no movement, we are done. *)
            if classify_float d = FP_zero then () else
            let x = info.x +. d *. cos v_theta in
            let y = info.y -. d *. sin v_theta in
            (* Check for collision with wall *)
            if Quadtree.collides info.x info.y x y info.r qtree <> None then begin
              log "arena" 6 (sprintf "Failed move to (%f, %f) -- wall" x y);
              info.v <- 0.0, v_theta;
            end
            (* Check for collision with another object *)
            else if ObjMap.fold
                    (fun obj2 info2 cancel ->
                      cancel ||
                      (Geometry.dist (x, y) (info2.x, info2.y))
                       < info.r +. info2.r &&
                      coll obj obj2)
                    (ObjMap.remove obj objs)
                    false then begin
              log "arena" 6 (sprintf "Failed move to (%f, %f) -- bot" x y);
              info.v <- 0.0, v_theta;
            end
            else begin
              log "arena" 9 (sprintf "Good move to (%f, %f)" x y);
              info.x <- x; info.y <- y;
            end
          end
        in
        ObjMap.iter step_obj objs
    end

end
