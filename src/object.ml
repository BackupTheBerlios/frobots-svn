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
let next_id = ref 0;;

let gensym () =
  let id = !next_id in
  incr next_id; id;;

class virtual obj ~(r: float) =
  object
    val id = gensym ()
    method virtual draw_onto :
      [`pixmap] GDraw.drawable -> x:int -> y:int ->
        theta:float -> scale:float -> unit
    method id = id
    method r = r
  end;;

class bot ~(team: int) ~(code: Absyn.exp) ~(env: Env.env) =
  let team_color =
    match team with
    | 0 -> "red"
    | 1 -> "blue"
    | 2 -> "green"
    | 3 -> "orange"
    | 4 -> "purple"
    | 5 -> "yellow"
    | _ -> "black" in
  object
    inherit obj 25.0
    val mutable energy = 1.0  (* invariant: 0.0 < energy <= 1.0 *)
    val mutable state : Eval.state = Eval.init_state env code
    val mutable turret = 0.0
    method draw_onto (drawable: [`pixmap] GDraw.drawable)
                     ~(x: int) ~(y: int) ~(theta: float) ~(scale: float) =
      drawable#set_foreground (`NAME team_color);
      let pixels = 25.0 /. scale in
      (* body of tank *)
      let alpha = theta +. 0.5 in
      let c = cos alpha and s = sin alpha in
      let p0x = x + int_of_float (c *. pixels)
      and p0y = y + int_of_float ((-. s) *. pixels) in
      drawable#line x y p0x p0y;
      let alpha = theta -. 0.5 in
      let c = cos alpha and s = sin alpha in
      let p1x = x + int_of_float (c *. pixels)
      and p1y = y + int_of_float ((-. s) *. pixels) in
      drawable#line x y p1x p1y;
      let alpha = Const.pi +. theta +. 0.7 in
      let c = cos alpha and s = sin alpha in
      let p2x = x + int_of_float (c *. pixels)
      and p2y = y + int_of_float ((-. s) *. pixels) in
      drawable#line x y p2x p2y;
      let alpha = Const.pi +. theta -. 0.7 in
      let c = cos alpha and s = sin alpha in
      let p3x = x + int_of_float (c *. pixels)
      and p3y = y + int_of_float ((-. s) *. pixels) in
      drawable#line x y p3x p3y;
      drawable#polygon ~filled:false [p0x, p0y; p1x, p1y; p2x, p2y; p3x, p3y];
      (* turret *)
      let alpha = theta +. turret in
      let c = cos alpha and s = sin alpha in
      let px = x + int_of_float (c *. 0.9 *. pixels)
      and py = y + int_of_float ((-. s) *. 0.9 *. pixels) in
      drawable#line x y px py
    method energy = energy
    method set_energy e = energy <- e
    method state = state
    method set_state st = state <- st
    method turret = turret
    method set_turret t = turret <- t
  end;;

class shell =
  object
    inherit obj 5.0
    method draw_onto (drawable: [`pixmap] GDraw.drawable)
                     ~(x: int) ~(y: int) ~(theta: float) ~(scale: float) =
      drawable#set_foreground (`NAME "white");
      let pixels = 5.0 /. scale in
      let c = cos theta and s = sin theta in
      let dx = int_of_float ((c +. s) *. pixels)
      and dy = int_of_float ((c -. s) *. pixels) in
      drawable#line (x - dx) (y - dy) (x + dx) (y + dy)
  end;;

type t = obj;;

let compare o1 o2 =
  compare o1#id o2#id;;
