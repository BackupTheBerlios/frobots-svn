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

open Absyn;;
open Env;;
open Eval;;

let returned_value = ref (IntV 0);;

module E = Eval.Make (struct type aux = unit end);;

E.register_builtin "return"
  (fun vs _ ->
    match vs with
    | [v] -> returned_value := v; IntV 0
    | _ -> failwith "return takes one argument");;

begin
  let lexbuf = Lexing.from_channel stdin in
  let e = Parser.main Lexer.token lexbuf in
  let en = insert "return" (BuiltinV "return") empty_env in
  let en = List.fold_left (fun en (b, f) -> E.register_builtin b f;
                                            insert b (BuiltinV b) en)
                          en Builtins.arrays in
  ignore (E.eval en () e);
  let v1 = !returned_value in
  let rec loop state =
    match E.step state () with
    | None -> ()
    | Some state' -> loop state' in
  loop (init_state en e);
  if v1 = !returned_value then
    exit 0
  else begin
    printf "--------\n%s\n----\n%s\n--------\n"
           (pp_value v1) (pp_value !returned_value);
    exit 1
  end
end
