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
type var = string

type unop = Neg | Not

type binop = Plus | Minus | Times | Div | And | Or
           | Equal | NotEqual | Lt | Gt | Lte | Gte

type exp = IntE of int
         | FloatE of float
         | CharE of char
         | StringE of string
         | IdE of var
         | UnopE of unop * exp
         | BinopE of exp * binop * exp
         | IfE of exp * exp * exp
         | AppE of exp * exp list
         | FunE of var list * exp
         | LetE of var * exp * exp
         | LetrecE of (var * var list * exp) list * exp
         | RefE of exp
         | AssignE of var * exp
         | DerefE of exp
         | SeqE of exp * exp
