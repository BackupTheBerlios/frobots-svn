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
type wall = (float * float) * (float * float)
val wall : float -> float -> float -> float -> wall

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

module Make :
  functor (O: sig type t val compare : t -> t -> int end) ->
    (S with type obj = O.t)
