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
open Absyn;;

exception Unbound_id of Absyn.var;;

let unbound x =
  raise (Unbound_id x);;

module StringSet = Set.Make (String);;

let rec check (s: StringSet.t) = function
  | IntE _ | FloatE _ | CharE _  | StringE _ -> ()
  | IdE x -> if not (StringSet.mem x s) then unbound x else ()
  | UnopE (_, e) -> check s e
  | BinopE (e1, _, e2) -> check s e1; check s e2
  | IfE (e, e1, e2) -> check s e; check s e1; check s e2
  | AppE (e, es) -> check s e; List.iter (check s) es
  | FunE (xs, e) -> check (List.fold_right StringSet.add xs s) e
  | LetE (x, e1, e2) -> check s e1; check (StringSet.add x s) e2
  | LetrecE (fs, e) ->
      let s' = List.fold_left (fun s (f, _, _) -> StringSet.add f s) s fs in
      List.iter (fun (_, xs, e) ->
                  check (List.fold_right StringSet.add xs s') e) fs;
      check s' e
  | RefE e -> check s e
  | AssignE (x, e) -> if not (StringSet.mem x s) then unbound x else check s e
  | DerefE e -> check s e
  | SeqE (e1, e2) -> check s e1; check s e2;;

let id_check vars e =
  check (List.fold_right StringSet.add vars StringSet.empty) e;;
