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
open Env;;

(* For the operational interface, we maintain a continuation in the form of
   a control stack.  Note: for simplicity in [step], the evaluated argument
   list in [AppiF] is in reverse order. *)
type c_frame = UnopF of unop
             | Binop1F of env * binop * exp     (* first arg being eval'ed *)
             | Binop2F of value * binop         (* second arg being eval'ed *)
             | IfF of env * exp * exp
             | App0F of env * exp list                  (* fun being eval'ed *)
             | AppiF of env * value * value list * exp list     (* ith arg *)
             | LetF of env * var * exp
             | RefF
             | AssignF of value
             | DerefF
             | SeqF of env * exp;;

type c_stack = c_frame list;;

(* For each process, we maintain its control stack, the depth of the control
   stack, and either a pending expression or working value. *)
type curr_item = Exp of env * exp | Val of value;;
type state = c_stack * int * curr_item;;

let init_state en e =
  ([], 0, Exp (en, e));;

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

module Make (A: sig type aux end) = struct

type aux = A.aux;;

(* The following definition allows us to use [error] as we would [printf],
   then raise an exception with the produced string. *)
let error s =
  ignore (Printf.kprintf (fun t -> failwith t) s);
  failwith "This is not the exception you are looking for.  Move along.";;

let builtin_handlers : (string, value list -> aux -> value) Hashtbl.t =
  Hashtbl.create 97;;

let register_builtin (b: builtin) (f: value list -> aux -> value) =
  Hashtbl.add builtin_handlers b f;;

let apply_builtin b vs aux =
  try
    (Hashtbl.find builtin_handlers b) vs aux
  with
    Not_found -> error "Undefined builtin: %s" b;;

let bool_of_value = function
  | IntV i -> i <> 0
  | FloatV f ->
      begin match classify_float f with
      | (FP_subnormal | FP_zero) -> false
      | _ -> true
      end
  | _ -> error "attempting to use non-numeric value as boolean";;

let value_of_bool = function
  | true -> IntV 1
  | false -> IntV 0;;

let numerical_binop p v1 v2 =
  let wrapi f =
    (fun a b -> if f a b then 1 else 0) in
  let wrapf f =
    (fun a b -> if f a b then 1.0 else 0.0) in
  let op =
    match p with
    | Plus -> (( + ), ( +. ))
    | Minus -> (( - ), ( -. ))
    | Times -> (( * ), ( *. ))
    | Div -> (( / ), ( /. ))
    | Equal -> (wrapi ( = ), wrapf ( = ))
    | NotEqual -> (wrapi ( <> ), wrapf ( <> ))
    | Lt -> (wrapi ( < ), wrapf ( < ))
    | Gt -> (wrapi ( > ), wrapf ( > ))
    | Lte -> (wrapi ( <= ), wrapf ( <= ))
    | Gte -> (wrapi ( >= ), wrapf ( >= ))
    | (And | Or) -> assert false in
  match v1, p, v2 with
  | (IntV _ | FloatV _), Div, IntV 0 ->
      error "division by zero"
  | IntV i1, _, IntV i2 -> IntV (fst op i1 i2)
  | IntV i1, _, FloatV f2 -> FloatV (snd op (float_of_int i1) f2)
  | FloatV f1, _, IntV i2 -> FloatV (snd op f1 (float_of_int i2))
  | FloatV f1, _, FloatV f2 -> FloatV (snd op f1 f2)
  | _, _, _ ->
      error "numeric operation on non-numerical values";;

let rec eval en aux = function
  | IntE i -> IntV i
  | FloatE f -> FloatV f
  | CharE c -> CharV c
  | StringE s -> StringV s
  | IdE x -> lookup x en
  | UnopE (p, e) -> apply_unop en aux (p, e)
  | BinopE (e1, p, e2) -> apply_binop en aux (e1, p, e2)
  | IfE (e, e1, e2) ->
      eval en aux (if bool_of_value (eval en aux e) then e1 else e2)
  | AppE (e, es) -> apply_fun (eval en aux e) (List.map (eval en aux) es) aux
  | FunE (xs, e) -> ClosureV (xs, e, ref en)
  | LetE (x, e1, e2) ->
      let v = eval en aux e1 in
      let en' = insert x v en in
      eval en' aux e2
  | LetrecE (fs, e) ->
      let closures =
        List.map (fun (f, xs, e) -> ClosureV (xs, e, ref empty_env)) fs in
      let en' =
        List.fold_left2 (fun en (f, _, _) c -> insert f c en) en fs closures in
      List.iter (function ClosureV (_, _, en) -> en := en'
                        | _ -> assert false) closures;
      eval en' aux e
    (* In implementing state, we just use the heap of the host environment. *)
  | RefE e -> RefV (ref (eval en aux e))
  | AssignE (x, e) ->
      begin match lookup x en with
      | RefV r -> r := (eval en aux e); IntV 0
      | _ -> error "attempted to assign to non-ref value"
      end
  | DerefE e ->
      begin match eval en aux e with
      | RefV r -> !r
      | _ -> error "attempted to dereference non-ref value"
      end
  | SeqE (e1, e2) -> ignore (eval en aux e1); eval en aux e2

and apply_unop en aux (p, e) =
  match p, eval en aux e with
  | Neg, IntV i -> IntV (-i)
  | Neg, FloatV f -> FloatV (-.f)
  | Neg, _ -> error "negation applied to non-numeric value"
  | Not, v -> value_of_bool (not (bool_of_value v))

and apply_binop en aux (e1, p, e2) =
  match p with
  | And ->
      value_of_bool (bool_of_value (eval en aux e1) &&
                     bool_of_value (eval en aux e2))
  | Or ->
      value_of_bool (bool_of_value (eval en aux e1) ||
                     bool_of_value (eval en aux e2))
  | (Plus | Minus | Times | Div | Equal | NotEqual | Lt | Gt | Lte | Gte) ->
      numerical_binop p (eval en aux e1) (eval en aux e2)

(* The following function could really just be folded into [eval], but we keep
   it seperate to follow tradition. *)
and apply_fun f vs aux =
  match f with
    BuiltinV b -> apply_builtin b vs aux
  | ClosureV (xs, e, en) ->
      eval (List.fold_right2 insert xs vs !en) aux e
  | _ -> error "application of non-function";;

(* Warning: read this code at your own risk!  Or just take my word that there
   are no ``dirty tricks'' in here, only a bit of deep magic. -- Justin *)
let step (stk, n, curr) aux: state option =
  if n > 5000 then error "max stack depth reached (infinite recursion?)" else
  match stk, curr with
  | [], Val _ -> None (* terminal state *)
  | UnopF Neg :: stk', Val (IntV i) ->
      Some (stk', n - 1, Val (IntV (-i)))
  | UnopF Neg :: stk', Val (FloatV f) ->
      Some (stk', n - 1, Val (FloatV (-.f)))
  | UnopF Neg :: _, Val _ -> error "negation applied to non-numeric value"
  | UnopF Not :: stk', Val v ->
      Some (stk', n - 1, Val (value_of_bool (not (bool_of_value v))))
  | Binop1F (en, And, e2) :: stk', Val v1 ->
      Some (stk', n - 1, if bool_of_value v1 then Exp (en, e2)
                                             else Val (IntV 0))
  | Binop1F (en, Or, e2) :: stk', Val v1 ->
      Some (stk', n - 1, if bool_of_value v1 then Val (IntV 1)
                                             else Exp (en, e2))
  | Binop2F (_, (And | Or)) :: _, Val _ -> assert false
  | Binop1F (en, p, e2) :: stk', Val v1 ->
      Some (Binop2F (v1, p) :: stk', n, Exp (en, e2))
  | Binop2F (v1, p) :: stk', Val v2 ->
      Some (stk', n - 1, Val (numerical_binop p v1 v2))
  | IfF (en, e1, e2) :: stk', Val v ->
      Some (stk', n - 1, Exp (en, if bool_of_value v then e1 else e2))
  | App0F (_, []) :: stk', Val (BuiltinV b) ->
      Some (stk', n - 1, Val (apply_builtin b [] aux))
  | App0F (_, []) :: stk', Val (ClosureV ([], e, en)) ->
      Some (stk', n - 1, Exp (!en, e))
  | App0F (_, []) :: _, Val (ClosureV _) ->
      error "function expects at least one argument"
  | App0F (_, []) :: _, Val _ ->
      error "application of non-function"
  | App0F (en, e :: es) :: stk', Val f ->
      Some (AppiF (en, f, [], es) :: stk', n, Exp (en, e))
  | AppiF (_, BuiltinV b, vs, []) :: stk', Val vn ->
      Some (stk', n - 1, Val (apply_builtin b (List.rev (vn :: vs)) aux))
  | AppiF (_, ClosureV (xs, e, en), vs, []) :: stk', Val vn ->
      let vs = List.rev (vn :: vs) in
      let en' = List.fold_right2 insert xs vs !en in
      Some (stk', n - 1, Exp (en', e))
  | AppiF (_, _, _, []) :: _, Val vn ->
      error "application of non-function"
  | AppiF (en, f, vs, e :: es) :: stk', Val v ->
      Some (AppiF (en, f, v :: vs, es) :: stk', n, Exp (en, e))
  | LetF (en, x, e) :: stk', Val v ->
      Some (stk', n - 1, Exp (insert x v en, e))
  | RefF :: stk', Val v ->
      Some (stk', n - 1, Val (RefV (ref v)))  (* use the host system's heap *)
  | AssignF (RefV r) :: stk', Val v ->
      r := v;
      Some (stk', n - 1, Val (IntV 0))
  | AssignF _ :: _, Val _ ->
      error "assignment to non-ref value"
  | DerefF :: stk', Val (RefV r) ->
      Some (stk', n - 1, Val !r)
  | DerefF :: _, Val _ ->
      error "attempt to dereference non-ref value"
  | SeqF (en, e) :: stk', Val _ ->
      Some (stk', n - 1, Exp (en, e))
  | _, Exp (_, IntE i) -> Some (stk, n, Val (IntV i))
  | _, Exp (_, FloatE f) -> Some (stk, n, Val (FloatV f))
  | _, Exp (_, CharE c) -> Some (stk, n, Val (CharV c))
  | _, Exp (_, StringE s) -> Some (stk, n, Val (StringV s))
  | _, Exp (en, IdE x) -> Some (stk, n, Val (lookup x en))
  | _, Exp (en, UnopE (p, e)) ->
      Some (UnopF p :: stk, n + 1, Exp (en, e))
  | _, Exp (en, BinopE (e1, p, e2)) ->
      Some (Binop1F (en, p, e2) :: stk, n + 1, Exp (en, e1))
  | _, Exp (en, IfE (e, e1, e2)) ->
      Some (IfF (en, e1, e2) :: stk, n + 1, Exp (en, e))
  | _, Exp (en, AppE (f, es)) ->
      Some (App0F (en, es) :: stk, n + 1, Exp (en, f))
  | _, Exp (en, FunE (xs, e)) ->
      Some (stk, n, Val (ClosureV (xs, e, ref en)))
  | _, Exp (en, LetE (x, e1, e2)) ->
      Some (LetF (en, x, e2) :: stk, n + 1, Exp (en, e1))
  | _, Exp (en, LetrecE (fs, e)) ->
      let closures =
        List.map (fun (f, xs, e) -> ClosureV (xs, e, ref empty_env)) fs in
      let en' =
        List.fold_left2 (fun en (f, _, _) c -> insert f c en) en fs closures in
      List.iter (function ClosureV (_, _, en) -> en := en'
                        | _ -> assert false) closures;
      Some (stk, n, Exp (en', e))
  | _, Exp (en, RefE e) ->
      Some (RefF :: stk, n + 1, Exp (en, e))
  | _, Exp (en, AssignE (x, e)) ->
      Some (AssignF (lookup x en) :: stk, n + 1, Exp (en, e))
  | _, Exp (en, DerefE e) ->
      Some (DerefF :: stk, n + 1, Exp (en, e))
  | _, Exp (en, SeqE (e1, e2)) ->
      Some (SeqF (en, e2) :: stk, n + 1, Exp (en, e1));;

end
