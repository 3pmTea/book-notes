%{
#include <string.h>
#include "util.h"
#include "tokens.h"
#include "errormsg.h"
#define STRING_LIMIT 1024

int charPos = 1;
static int commentLv = 0;
static char buf[STRING_LIMIT];
static int bufPos = 0;

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

void bufReset()
{
    bufPos = 0;
}

void bufFinish()
{
    buf[bufPos] = '\0';
}

void bufAppend(char c)
{
    buf[bufPos++] = c;
}

%}

digit   [0-9]
letter  [a-zA-Z]
id      {letter}[_a-zA-Z0-9]*
space   [\t ]
%x      comment
%x      string
%%

<comment>"*/"   {adjust(); if (--commentLv == 0) BEGIN(INITIAL);}
<comment>"/*"   {adjust(); commentLv++;}
<comment>\n     {EM_newline();}
<comment>.      {adjust();}
<string>\"      {adjust(); bufFinish(); yylval.sval=strdup(buf); BEGIN(INITIAL); return STRING;}
<string>\n      {EM_error(EM_tokPos, "LEX ERROR: unfinished string."); EM_newline(); yymore();}
<string>\\[ \t\n\r]+\\  {yymore();}
<string>\\t     {bufAppend('\t'); yymore();}
<string>\\n     {bufAppend('\n'); yymore();}
<string>\\\\    {bufAppend('\\'); yymore();}
<string>\\\"    {bufAppend('\"'); yymore();}
<string>\\{digit}{3}    {
                            int n = atoi(yytext+1);
                            if (n>0 && n<256) {
                                bufAppend((char)n);
                                yymore();
                            } else {
                                EM_error(EM_tokPos, "invalid ascii code");
                            }
                            yymore();
                        }
<string>.   {bufAppend(yytext[yyleng-1]); yymore();}

{space}     {adjust();}
\n	        {EM_newline();}
\r          {}
"/*"        {adjust(); commentLv++; BEGIN(comment);}
\"          {adjust(); bufReset(); BEGIN(string);}
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

.	        {adjust(); EM_error(EM_tokPos,"illegal token: %s", yytext);}


