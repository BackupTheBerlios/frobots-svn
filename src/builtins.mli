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
type 'a t = Env.builtin * (Env.value list -> 'a -> Env.value)

exception Builtin of string

(* Builtins are seperated into different groups for clarity and so that a
   custom environment (for testing, etc.) can be obtained.  To include
   everything, use either the [all_nodebug] or [all] value specified below. *)

(* An array data type. *)
val arrays : 'a t list;;

(* Operations fundamental to playing the game. *)
val core : Game_state.t t list;;

(* Environment suitable for tournament play. *)
val all_nodebug : Game_state.t t list;;

(* Functions for debugging of bots. *)
val debug : 'a t list;;

(* Everything. *)
val all : Game_state.t t list;;
