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
let out = ref stderr;;

module StringMap = Map.Make (String);;
let view : int StringMap.t ref = ref StringMap.empty;;

let log (t: string) (p: int) (s: string) =
  if StringMap.mem t !view && StringMap.find t !view >= p ||
     StringMap.mem "ALL" !view && StringMap.find "ALL" !view >= p then begin
    Printf.fprintf !out "(%d) %s: %s\n" p t s;
    flush !out;
  end;;

let log_lazy (t: string) (p: int) (s: string Lazy.t) =
  if StringMap.mem t !view && StringMap.find t !view >= p ||
     StringMap.mem "ALL" !view && StringMap.find "ALL" !view >= p then begin
    Printf.fprintf !out "(%d) %s: %s\n" p t (Lazy.force s);
    flush !out;
  end;;

let send_to (o: out_channel) =
  out := o;;

let view_upto (t: string) (p: int) =
  view := StringMap.add t p !view;;
