%{
#include <stdio.h>
#include "util.h"
#include "errormsg.h"

int yylex(); /* function prototype */

void yyerror(char *s)
{
 EM_error(EM_tokPos, "%s", s);
}
%}


%union {
	int pos;
	int ival;
	string sval;
}

%token <sval> ID STRING
%token <ival> INT

%token 
  COMMA COLON SEMICOLON LPAREN RPAREN LBRACK RBRACK 
  LBRACE RBRACE DOT 
  PLUS MINUS TIMES DIVIDE EQ NEQ LT LE GT GE
  AND OR ASSIGN
  ARRAY IF THEN ELSE WHILE FOR TO DO LET IN END OF 
  BREAK NIL
  FUNCTION VAR TYPE 

%start program

%%

program:  exp
        ;

decs:
        | decs dec
        ;

dec:    tydec
      | vardec
      | fundec
      ;

tydec:  TYPE typeid EQ ty
      ;

ty:     typeid
      | LBRACE tyfields RBRACE
      | ARRAY OF typeid
      ;

tyfields:   
            | ID COLON typeid restfields
            ;

restfields: 
          | restfields COMMA ID COLON typeid
          ;

vardec:   VAR ID ASSIGN exp
        | VAR ID COLON typeid ASSIGN exp
        ;

fundec:   FUNCTION ID LPAREN tyfields RPAREN EQ exp
        | FUNCTION ID LPAREN tyfields RPAREN COLON typeid EQ exp
        ;

typeid:   ID
        ;

exp:      lvalue
        | NIL
        | INT
        | MINUS INT
        | exp op exp
        | STRING
        | typeid LBRACE fieldinitlist RBRACE
        | subscript OF exp
        | lvalue ASSIGN exp
        | IF exp THEN exp ELSE exp
        | IF exp THEN exp
        | WHILE exp DO exp
        | FOR ID ASSIGN exp TO exp DO exp
        | BREAK
        | LET decs IN expseq END
        | LPAREN expseq RPAREN
        ;

fieldinitlist:
                | fieldinits ID EQ exp
                ;
                
fieldinits:
                | fieldinits ID EQ exp COMMA
                ;

expseq:
                | exps exp
                ;

exps:           
                | exps exp SEMICOLON
                ;

lvalue:   ID
        | lvalue DOT ID
        | subscript
        ;

subscript:      ID LBRACK exp RBRACK
              ;
        
op:     PLUS
      | MINUS
      | TIMES
      | DIVIDE
      | EQ
      | NEQ
      | LT
      | GT
      | LE
      | GE
      | AND
      | OR
      ;