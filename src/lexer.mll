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
{
  open Parser;;
  let eme = Lexing.lexeme;;

  let kw_table = Hashtbl.create 53;;
  let _ =
    List.iter (fun (kw, tok) -> Hashtbl.add kw_table kw tok)
              ["let", KW_let;
               "in", KW_in;
               "rec", KW_rec;
               "and", KW_and;
               "ref", KW_ref;
               "fun", KW_fun;
               "begin", KW_begin;
               "end", KW_end;
               "for", KW_for;
               "to", KW_to;
               "while", KW_while;
               "do", KW_do;
               "done", KW_done;
               "not", KW_not;
               "true", KW_true;
               "false", KW_false;
               "if", KW_if;
               "then", KW_then;
               "else", KW_else];;

  let make_kw s =
    try
      Hashtbl.find kw_table s
    with
      Not_found -> ID s;;

  let char_of_string s =
    match String.length s, s.[0] with
      1, c -> c
    | 2, '\\' ->
        begin match s.[1] with
          '\\' -> '\\'
        | '"' -> '"'
        | 'n' -> '\n'
        | 't' -> '\t'
        | _ -> failwith "lexer: Unrecognized escape sequence"
        end
    | 4, '\\' ->
        failwith "lexer: trigraphs not yet implemented"  (* TODO *)
    | _, _ ->
        assert false;;

  let buf = Buffer.create 80;;

  let start_string () =
    Buffer.clear buf;;

  let add_to_string s =
    Buffer.add_char buf (char_of_string s);;

  let finish_string () =
    Buffer.contents buf;;
}

let lower = ['a'-'z']
let upper = ['A'-'Z']
let alpha = lower | upper
let num = ['0'-'9']
let alphanum = alpha | num
let schar = [^ '\'' '"'] | '\\' num num num | "\\\\" | "\\\"" | "\\t" | "\\n"

rule token = parse
  [' ' '\t' '\n']  { token lexbuf }
| "(*" ([^ '*'] | ('*'+ [^ '*' ')']))* '*'* "*)"  { token lexbuf }
| '('  { LPAREN }
| ')'  { RPAREN }
| ','  { COMMA }
| '='  { EQUAL }
| "<>"  { NEQUAL }
| "<"  { LT }
| ">"  { GT }
| "<="  { LTE }
| ">="  { GTE }
| "->"  { ARROW }
| '_'  { USCORE }
| ':'  { COLON }
| ';'  { SEMI }
| '['  { LBRACK }
| ']'  { RBRACK }
| '*'  { STAR }
| '/'  { SLASH }
| '+'  { PLUS }
| '-'  { MINUS }
| "&&"  { AND }
| "||"  { OR }
| '|'  { PIPE }
| ":="  { ASSIGN }
| '!'  { BANG }
| '-'? ('0' | ['1'-'9'] num*)  { INT (int_of_string (eme lexbuf)) }
| '-'? ('0' | ['1'-'9'] num*) '.' num*  { FLOAT (float_of_string (eme lexbuf)) }
| "'\\''"  { CHAR '\'' }
| "'\\t'"  { CHAR '\t' }
| "'\\n'"  { CHAR '\n' }
| '\'' _ '\''  { CHAR (let s = eme lexbuf in s.[1]) }
| '"'  { start_string (); str lexbuf }
| lower (alphanum | ['\'' '_' '?'])*  { make_kw (eme lexbuf) }
| upper (alphanum | ['\'' '_' '?'])*  { BIGID (eme lexbuf) }
| eof  { EOF }
| _  { failwith ("lexer: Unrecognized character '" ^ (eme lexbuf) ^ "'") }

and str = parse
  schar  { add_to_string (eme lexbuf); str lexbuf }
| '"'  { STRING (finish_string ()) }
| '\\' _  { failwith ("lexer: Bad escape sequence " ^ (eme lexbuf) ^ "\"") }
