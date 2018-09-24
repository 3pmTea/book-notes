%{
#include <string.h>
#include "util.h"
#include "tokens.h"
#include "errormsg.h"

int charPos = 1;

int yywrap(void)
{
    charPos = 1;
    return 1;
}

void adjust(void)
{
    EM_tokPos = charPos;
    charPos += yyleng;
}

%}

digit   [0-9]
letter  [a-zA-Z]
id      {letter}[_a-zA-Z0-9]*
space   [\t ]
%x      comment
%x      string
%%

<comment>"*/"   {adjust(); BEGIN(INITIAL);}
<comment>.|\n   {adjust(); continue;}
<string>\"  {adjust(); BEGIN(INITIAL); return STRING;}
<string>.   {adjust(); yymore();}

{space}     {adjust(); continue;}
\n	        {adjust(); EM_newline(); continue;}
\"          {adjust(); yylval.sval = yytext; BEGIN(string);}
","	        {adjust(); return COMMA;}
":"         {adjust(); return COLON;}
";"         {adjust(); return SEMICOLON;}
"("         {adjust(); return LPAREN;}
")"         {adjust(); return RPAREN;}
"["         {adjust(); return LBRACK;}
"]"         {adjust(); return RBRACK;}
"{"         {adjust(); return LBRACE;}
"}"         {adjust(); return RBRACE;}
"."         {adjust(); return DOT;}
"+"         {adjust(); return PLUS;}
"-"         {adjust(); return MINUS;}
"*"         {adjust(); return TIMES;}
"/"         {adjust(); return DIVIDE;}
"="         {adjust(); return EQ;}
"<>"        {adjust(); return NEQ;}
"<"         {adjust(); return LT;}
"<="        {adjust(); return LE;}
">"         {adjust(); return GT;}
">="        {adjust(); return GE;}
"&"         {adjust(); return AND;}
"|"         {adjust(); return OR;}
":="        {adjust(); return ASSIGN;}
"/*"        {adjust(); BEGIN(comment);}

array       {adjust(); return ARRAY;}
if          {adjust(); return IF;}
then        {adjust(); return THEN;}
else        {adjust(); return ELSE;}
while       {adjust(); return WHILE;}
for  	    {adjust(); return FOR;}
to          {adjust(); return TO;}
do          {adjust(); return DO;}
let         {adjust(); return LET;}
in          {adjust(); return IN;}
end         {adjust(); return END;}
of          {adjust(); return OF;}
break       {adjust(); return BREAK;}
nil         {adjust(); return NIL;}
function    {adjust(); return FUNCTION;}
var         {adjust(); return VAR;}
type        {adjust(); return TYPE;}
[0-9]+	    {adjust(); yylval.ival = atoi(yytext); return INT;}
{id}        {adjust(); yylval.sval = yytext; return ID;}
.	        {adjust(); EM_error(EM_tokPos,"illegal token");}


