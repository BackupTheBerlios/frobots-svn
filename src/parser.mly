%{
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
  open Absyn
%}
%token KW_let KW_in KW_rec KW_and KW_ref KW_fun KW_begin KW_end KW_for KW_to
%token KW_while KW_do KW_done KW_not KW_true KW_false KW_if KW_then KW_else
%token LPAREN RPAREN COMMA EQUAL ARROW USCORE COLON SEMI LBRACK RBRACK STAR
%token SLASH PLUS MINUS UMINUS AND OR PIPE ASSIGN BANG NEQUAL LT GT LTE GTE
%token <int> INT
%token <float> FLOAT
%token <char> CHAR
%token <string> STRING
%token <string> ID
%token <string> BIGID
%token EOF
%right ASSIGN
%left OR
%left AND
%left EQUAL NEQUAL LT GT LTE GTE
%left PLUS MINUS
%left STAR SLASH
%nonassoc UMINUS
%nonassoc KW_not
%nonassoc BANG
%start main
%type <Absyn.exp> main
%%
main:
    expr EOF            { $1 }
;
expr:
    stmt                                { $1 }
  | stmt SEMI                           { $1 }
  | stmt SEMI expr                      { SeqE ($1, $3) }
  | KW_fun LPAREN RPAREN ARROW expr     { FunE ([], $5) }
  | KW_fun LPAREN idlist RPAREN ARROW expr      { FunE ($3, $6) }
  | KW_let ID EQUAL expr KW_in expr     { LetE ($2, $4, $6) }
  | KW_let KW_rec defn andlist KW_in expr       { LetrecE ($3 :: $4, $6) }
;
idlist:
    ID               { [$1] }
  | ID COMMA idlist  { $1 :: $3 }
;
andlist:
    KW_and defn andlist         { $2 :: $3 }
  |                             { [] }
;
defn:
    ID LPAREN idlist RPAREN EQUAL expr  { ($1, $3, $6) }
;
/* see Aho, Sethi, and Ullman (dragon book) page 178 */
stmt:
    matched_stmt        { $1 }
  | unmatched_stmt      { $1 }
;
matched_stmt:
    opexp               { $1 }
  | KW_if expr KW_then matched_stmt KW_else matched_stmt
                        { IfE ($2, $4, $6) }
;
unmatched_stmt:
    KW_if expr KW_then matched_stmt     { IfE ($2, $4, IntE 0) }
  | KW_if expr KW_then matched_stmt KW_else unmatched_stmt
                        { IfE ($2, $4, $6) }
;
opexp:
    appseq              { $1 }
  | opexp PLUS opexp    { BinopE ($1, Plus, $3) }
  | opexp MINUS opexp   { BinopE ($1, Minus, $3) }
  | opexp STAR opexp    { BinopE ($1, Times, $3) }
  | opexp SLASH opexp   { BinopE ($1, Div, $3) }
  | MINUS opexp %prec UMINUS    { UnopE (Neg, $2) }
  | opexp AND opexp     { BinopE ($1, And, $3) }
  | opexp OR opexp      { BinopE ($1, Or, $3) }
  | KW_not opexp        { UnopE (Not, $2) }
  | ID ASSIGN opexp     { AssignE ($1, $3) }
  | BANG opexp          { DerefE $2 }
  | opexp EQUAL opexp   { BinopE ($1, Equal, $3) }
  | opexp NEQUAL opexp  { BinopE ($1, NotEqual, $3) }
  | opexp LT opexp      { BinopE ($1, Lt, $3) }
  | opexp GT opexp      { BinopE ($1, Gt, $3) }
  | opexp LTE opexp     { BinopE ($1, Lte, $3) }
  | opexp GTE opexp     { BinopE ($1, Gte, $3) }
;
appseq:
    atexpr                              { $1 }
  | appseq LPAREN RPAREN                { AppE ($1, []) }
  | appseq atexpr                       { AppE ($1, [$2]) }
  | appseq LPAREN exprlist RPAREN       { AppE ($1, $3) }
;
exprlist:
    expr COMMA expr     { [$1; $3] }
  | expr COMMA exprlist { $1 :: $3 }
;
atexpr:
    const                       { $1 }
  | ID                          { IdE $1 }
    /* allow ref to be treated as a normal function */
  | KW_ref                      { FunE (["x"], RefE (IdE "x")) }
  | LPAREN expr RPAREN          { $2 }
  | KW_begin expr KW_end        { $2 }
;
const:
    KW_true     { IntE 1 }
  | KW_false    { IntE 0 }
  | INT         { IntE $1 }
  | FLOAT       { FloatE $1 }
;
