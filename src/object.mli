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
class virtual obj :
  r:float ->
  object
    (* scale is given in abstract units per pixel *)
    method virtual draw_onto :
      [`pixmap] GDraw.drawable -> x:int -> y:int ->
        theta:float -> scale:float -> unit
    method id : int
    method r : float
  end

class bot :
  team:int ->
  code:Absyn.exp ->
  env:Env.env ->
  object
    inherit obj
    method draw_onto :
      [`pixmap] GDraw.drawable -> x:int -> y:int ->
        theta:float -> scale:float -> unit
    method energy : float
    method set_energy : float -> unit
    method set_state : Eval.state -> unit
    method set_turret : float -> unit
    method state : Eval.state
    method turret : float
  end

class shell :
  object
    inherit obj
    method draw_onto :
      [`pixmap] GDraw.drawable -> x:int -> y:int ->
        theta:float -> scale:float -> unit
  end

type t = obj

val compare : t -> t -> int
