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
type builtin = string

type env

type value = IntV of int
           | FloatV of float
           | CharV of char
           | StringV of string
           | ArrayV of value array
           | RefV of value ref
           | BuiltinV of builtin
           | ClosureV of Absyn.var list * Absyn.exp * env ref
           | ContV of (unit -> value)  (* ok, so maybe this isn't a value... *)

(* Pretty-printing: *)
val pp_value : value -> string

val empty_env : env

val insert : Absyn.var -> value -> env -> env

val lookup : Absyn.var -> env -> value
