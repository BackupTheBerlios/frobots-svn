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
open Env;;

type 'a t = Env.builtin * (Env.value list -> 'a -> Env.value)

exception Builtin of string;;

(* The following definition allows us to use [error] as we would [printf],
   then raise an exception with the produced string. *)
let error s =
  ignore (Printf.kprintf (fun t -> raise (Builtin t)) s);
  failwith "This is not the exception you are looking for.  Move along.";;

let arrays = [
  "make_array",
  (fun vs aux ->
    match vs with
    | [IntV n] -> ArrayV (Array.make n (IntV 0))
    | [IntV n; v0] -> ArrayV (Array.make n v0)
    | [_] -> error "make_array takes an integer as its first argument"
    | _ -> error "make_array takes at most 2 arguments");
  "array_length",
  (fun vs aux ->
    match vs with
    | [ArrayV a] -> IntV (Array.length a)
    | [_] -> error "array_length takes an array as its argument"
    | _ -> error "array_length takes a single argument");
  "array_get",
  (fun vs aux ->
    match vs with
    | [ArrayV a; IntV i] ->
        (try a.(i) with
          Invalid_argument _ -> error "array_get: index out of bounds")
    | [ArrayV _; _] ->
        error "array_get takes an integer as its second argument"
    | [_; _] -> error "array_get takes an array as its first argument"
    | _ -> error "array_get takes two arguments");
  "array_set",
  (fun vs aux ->
    match vs with
    | [ArrayV a; IntV i; v] ->
        (try a.(i) <- v with
          Invalid_argument _ -> error "array_set: index out of bounds");
        IntV 0
    | [ArrayV _; _; _] ->
        error "array_set takes an integer as its second argument"
    | [_; _; _] -> error "array_set takes an array as its first argument"
    | _ -> error "array_set takes three arguments")
];;

open Game_state;;

let core = [
  "set_accel",
  (fun vs aux ->
    match vs with
    | [IntV i] ->
        aux.arena#modify_object (aux.curr_bot :> Object.t)
                                ~a:(float_of_int i) ();
        IntV 0
    | [FloatV f] ->
        aux.arena#modify_object (aux.curr_bot :> Object.t) ~a:f ();
        IntV 0
    | _ -> error "set_accel takes a single number");
  "set_steering",
  (fun vs aux ->
    match vs with
    | [IntV i] ->
        aux.arena#modify_object (aux.curr_bot :> Object.t)
                                ~omega:(float_of_int i) ();
        IntV 0
    | [FloatV f] ->
        aux.arena#modify_object (aux.curr_bot :> Object.t) ~omega:f ();
        IntV 0
    | _ -> error "set_steering takes a single number");
];;

let debug = [
  "print",
  (fun vs aux ->
    List.iter print_string (List.map pp_value vs);
    print_newline ();
    flush stdout;
    IntV 0)
];;

let all_nodebug =
  List.concat [arrays; core];;
let all =
  List.concat [debug; all_nodebug];;
