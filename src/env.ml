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
module StringMap = Map.Make (String);;

type builtin = string

type env = value StringMap.t

and value = IntV of int
          | FloatV of float
          | CharV of char
          | StringV of string
          | ArrayV of value array
          | RefV of value ref
          | BuiltinV of builtin
          | ClosureV of Absyn.var list * Absyn.exp * env ref

let rec pp_value = function
  | IntV i -> string_of_int i
  | FloatV f -> string_of_float f
  | CharV c -> String.make 1 'c'
  | StringV s -> s
  | ArrayV a -> "Array: ???"
  | RefV r -> "Ref: " ^ (pp_value !r)
  | BuiltinV b -> "Builtin: " ^ b
  | ClosureV (xs, e, en) -> "Closure: fun (" ^ (String.concat ", " xs) ^ ") = ?"

let empty_env : env = StringMap.empty;;

let insert = StringMap.add;;

let lookup x en =
  try
    StringMap.find x en
  with
    Not_found -> begin
      Logging.log "Env.lookup" 3 (Printf.sprintf "unbound variable %s" x);
      raise Not_found
    end;;
