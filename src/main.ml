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
Random.self_init ();;

(* Show all critical and error conditions by default. *)
Logging.view_upto "ALL" 3;;

module Eval = Eval.Make (struct type aux = Game_state.t end);;
open Env;;
open Game_state;;

let en = List.fold_left (fun en (b, f) -> Eval.register_builtin b f;
                                          insert b (BuiltinV b) en)
                        empty_env Builtins.all;;

let slurp_bot fname =
  try
    let f = open_in fname in
    let lexbuf = Lexing.from_channel f in
    let exp = Parser.main Lexer.token lexbuf in
    close_in f;
    exp
  with
  | Parsing.Parse_error -> begin
      Logging.log "slurp_bot" 0 (fname ^ "could not be parsed");
      exit 1
    end
  | _ -> begin
      Logging.log "slurp_bot" 0 ("Couldn't read a valid bot from " ^ fname);
      exit 1
    end

module B = Set.Make (struct
  type t = Object.bot
  let compare b1 b2 = Object.compare (b1 :> Object.t) (b2 :> Object.t)
end);;

let bot_set = ref B.empty;;

Arg.parse
[
  ("-d", Arg.Int (fun i -> Logging.view_upto "ALL" i), "\tshow debug info")
]
(fun s ->
  let c = slurp_bot s in
  let b =
    new Object.bot ~team:(B.cardinal !bot_set) ~code:c ~env:en in
  bot_set := B.add b !bot_set)
"Usage: bots <bot1> <bot2> ...";;

(* This must happen after argument parsing so logging works. *)
module A = Arena.Make (Object);;

let arena = new A.arena ~x:1000.0 ~y:1000.0
                        ~walls:[(100.0, 100.0), (100.0, 900.0);
                                (100.0, 100.0), (900.0, 100.0);
                                (100.0, 900.0), (900.0, 900.0);
                                (900.0, 100.0), (900.0, 900.0)] ();;

B.iter
  (fun b ->
    let x = 125.0 +. Random.float 750.0
    and y = 125.0 +. Random.float 750.0
    and theta = Random.float (Const.pi *. 2.0) in
    arena#add_object (b :> Object.t) ~x ~y ~r:25.0 ~v:(1.0, theta) ())
  !bot_set;;

(* Initialize GUI: *)
let locale = GMain.Main.init ();;
let root = GWindow.window ~kind:`TOPLEVEL ~title:"Frobots"
                          ~width:500 ~height:500
                          ~allow_shrink:false ~allow_grow:false
                          ~border_width:0 ();;

(* A container to hold GUI elements: *)
let vbox = GPack.vbox ~packing:root#add ~homogeneous:false ();;

(* Arena display: *)
let arena_da = GMisc.drawing_area ~packing:vbox#add ();;
let arena_draw = lazy (new GDraw.drawable arena_da#misc#window);;

(* Backing store for arena graphics: *)
let arena_pix = GDraw.pixmap ~width:500 ~height:500 ();;

let repaint_display () =
  let drawable = Lazy.force arena_draw in
  drawable#put_pixmap ~x:0 ~y:0 arena_pix#pixmap;;

begin
  (* These handlers terminate the application on root window close. *)
  ignore (root#event#connect#delete (fun _ -> false));
  ignore (root#connect#destroy (fun () -> GMain.quit ()));

  (* Also terminate on 'q'. *)
  ignore (root#event#connect#key_press
          (fun k ->
            if GdkEvent.Key.string k = "q" then
              root#destroy ();
            false));
 
  (* Clear the backing store. *)
  arena_pix#set_background `BLACK;
  arena_pix#set_foreground `BLACK;
  arena_pix#rectangle ~x:0 ~y:0 ~width:500 ~height:500 ~filled:true ();

  (* Repaint as needed. *)
  arena_da#event#connect#expose
    ~callback:(fun _ -> repaint_display (); false);
end;;

let update_display () =
  (* Clear the backing store. *)
  arena_pix#set_background `BLACK;
  arena_pix#set_foreground `BLACK;
  arena_pix#rectangle ~x:0 ~y:0 ~width:500 ~height:500 ~filled:true ();
  (* Draw walls. *)
  arena_pix#set_foreground `WHITE;
  arena_pix#rectangle ~x:50 ~y:50 ~width:400 ~height:400 ~filled:false ();
  (* Draw objects. *)
  arena#iter
    (fun obj ~x ~y ~r ~v:(vmag, theta) ~a ~omega ->
      let x_disp = int_of_float x / 2
      and y_disp = int_of_float y / 2 in
      obj#draw_onto (arena_pix :> [`pixmap] GDraw.drawable)
                    ~x:x_disp ~y:y_disp ~theta ~scale:2.0);
  repaint_display ();;

let rounds = ref Const.rounds_per_update;;

let game_loop () =
  if !rounds == Const.rounds_per_update then begin
    rounds := 0;
    update_display ();
  end
  else incr rounds;
  (* Step bot code: *)
  B.iter (fun b ->
           if B.mem b !bot_set then begin
             match Eval.step b#state {arena = arena; curr_bot = b} with
             | None -> ()
             | Some st' -> b#set_state st'
           end) !bot_set;
  (* Let "physics" run its course: *)
  arena#step_all (fun _ _ -> true);
  true;;

begin
  (* Set up a timer. *)
  ignore (GMain.Timeout.add ~ms:10 ~callback:game_loop);
  (* It's showtime.  lablgtk shows everything else by default. *)
  root#show ();
  GMain.main ()
end;;
