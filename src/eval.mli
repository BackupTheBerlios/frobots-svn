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
type state

val init_state : Env.env -> Absyn.exp -> state

module type S = sig
  type aux

  val register_builtin :
    Env.builtin -> (Env.value list -> aux -> Env.value) -> unit

  (* The following [eval] (sometimes called ``downto'') is useful for
     testing. *)
  val eval : Env.env -> aux -> Absyn.exp -> Env.value

  (* More suitable for time-sharing is an operational interface:
     [step] returns [Some newstate] if progress can be made, and [None]
     if the current state is terminal. *)
  val step : state -> aux -> state option
end

module Make (A: sig type aux end) : (S with type aux = A.aux)
