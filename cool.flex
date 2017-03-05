/*
 *  The scanner definition for COOL.
 *  03/04/2017 Pietro Paolini : general.2.pulsarpietro@spamgourmet.com
 *
 */
%option yylineno debug
%x COMMENT
%x STRING
/*
 *  Stuff enclosed in %{ %} in the first section is copied verbatim to the
 *  output, so headers and global definitions are placed here to be visible
 * to the code in the file.  Don't remove anything that was here initially
 */
%{
#include <cool-parse.h>
#include <stringtab.h>
#include <utilities.h>

/* The compiler assumes these identifiers. */
#define yylval cool_yylval
#define yylex  cool_yylex

/* Max size of string constants */
#define MAX_STR_CONST 1025
#define YY_NO_UNPUT   /* keep g++ happy */

extern FILE *fin; /* we read from this file */

/* define YY_INPUT so we read from the FILE fin:
 * This change makes it possible to use this scanner in
 * the Cool compiler.
 */
#undef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( (result = fread( (char*)buf, sizeof(char), max_size, fin)) < 0) \
		YY_FATAL_ERROR( "read() in flex scanner failed");

char string_buf[MAX_STR_CONST]; /* to assemble string constants */
char *string_buf_ptr;

extern int curr_lineno;
extern int verbose_flag;

extern YYSTYPE cool_yylval;

/*
 *  Add Your own definitions here
 */
int nested_comment = 0;

#define YY_FATAL_ERROR(str) { fprintf(stderr, "Error : %s\n", str); }
#define STR_OKAY          0
#define STR_ERROR_NEWLINE 1
#define STR_ERROR_NULL    2      
#define IS_END_OF_STRING(h,l) (h == '\n' || h != '\\' && l == '\"')
int append(char * buffer) 
{
  int length = strlen(buffer);
  if ((string_buf_ptr + length) - string_buf >= MAX_STR_CONST) {
    cool_yylval.error_msg = "String constant too long";
    return -1;
  }
  strncpy(string_buf_ptr, buffer, length);
  string_buf_ptr += length;
  return 0;
}
%}

/*
 * Define names for regular expressions here.
 */

DARROW          =>
LE              <=
ASSIGN          <-
/* Keywords */

CLASS          (?i:class)
ELSE           (?i:else)
FI             (?i:fi)
IF             (?i:if)
IN             (?i:in)
INHERITS       (?i:inherits)
ISVOID         (?i:isvoid)
LET            (?i:let)
LOOP           (?i:loop)
POOL           (?i:pool)
THEN           (?i:then)
WHILE          (?i:while)
CASE           (?i:case)
ESAC           (?i:esac)
NEW            (?i:new)
OF             (?i:of)
NOT            (?i:not)
INT_CONST      [0-9]+

/* Boolean */

BOOLEAN_TRUE     t[rR][uU][eE]
BOOLEAN_FALSE    f[aA][lL][sS][eE]

/* String */
STR_CONST      \"

/* Comments */
COMMENT_INLINE "--".*
COMMENT_OPENING "(*"
COMMENT_CLOSING "*)"

/* Identifier */
TYPEID [A-Z][A-Za-z0-9_]*
OBJECTID [a-z][A-Za-z0-9_]*
/* White Space */
WHITE_SPACE (" "|\n|\f|\r|\t|\v)+
%%

 /*
  *  Nested comments
  */


\n {
  curr_lineno++;
}
 /*
  *  The multiple-character operators.
  */
{ASSIGN}                { return (ASSIGN); }
{DARROW}                { return (DARROW); }
{LE}                    { return (LE); }
{CLASS}                 { return (CLASS); }
{ELSE}                  { return (ELSE); }
{FI}                    { return (FI); }
{IF}                    { return (IF); }
{IN}                    { return (IN); }
{INHERITS}              { return (INHERITS); }
{ISVOID}                { return (ISVOID); }
{LET}                   { return (LET); }
{LOOP}                  { return (LOOP); }
{POOL}                  { return (POOL); }
{THEN}                  { return (THEN); }
{WHILE}                 { return WHILE; }
{CASE}                  { return CASE; }
{ESAC}                  { return ESAC;}
{NEW}                   { return NEW;}
{OF}                    { return OF;}
{NOT}                   { return NOT;}
{COMMENT_INLINE}        { ; }
{COMMENT_OPENING} {
  BEGIN COMMENT;
  nested_comment = 1;
}
{COMMENT_CLOSING} {
  cool_yylval.error_msg = "Unmatched *)";
  return ERROR;
}
<COMMENT>[^*|\n]*{COMMENT_OPENING} {
   nested_comment++;
}
<COMMENT>{COMMENT_CLOSING} {
  if (--nested_comment == 0)
    BEGIN INITIAL;
}
<COMMENT>([^*|\n])+               { ; }        
<COMMENT>\n                       { curr_lineno++; }
<COMMENT>\*\n                     { curr_lineno++; }
<COMMENT>\*[^\)]                  { ; }
<COMMENT><<EOF>> {
  BEGIN INITIAL;
  cool_yylval.error_msg = "EOF in comment";
  return ERROR;
}
{BOOLEAN_TRUE} {
  cool_yylval.boolean = true;
  return (BOOL_CONST); 
}
{BOOLEAN_FALSE} {
  cool_yylval.boolean = false;
  return (BOOL_CONST); 
}

 /*
  * Beginning of the string
  */
{STR_CONST} {
  /* yy_flex_debug = 1; */
  BEGIN STRING;
  string_buf_ptr = string_buf;
}
<STRING>[^\n\"\\\0]* {
  if (append(yytext))
    return ERROR;
}
<STRING>\\b|\\t|\\n|\\f|\\\\|\\\"|\\0 {
  char escaped_char[2] = { 0 };
  switch (yytext[1]) {
  case 'b':
    escaped_char[0] = '\b';
    break;
  case 't':
    escaped_char[0] = '\t';
    break;
  case 'n':
    escaped_char[0] = '\n';
    break;
  case 'f':
    escaped_char[0] = '\f';
    break;
  case '\\':
    escaped_char[0] = '\\';
    break;
  case '\"':
    escaped_char[0] = '\"';
    break;
  case '0':
    escaped_char[0] = '0';
    break;
  default:
      cool_yylval.error_msg = "The function I wrote is rubbish";
      return ERROR;
  }
  if (append(escaped_char)) {

    /* Why can't I use yyinput() in the definition sections */
    /* Head and loookahead char */
    int ch = 0, cl = yyinput(), res;
    while (true) {
      if (cl == EOF) {
	unput(cl);
	break;
      }
      if (IS_END_OF_STRING(ch, cl))
	break;
      ch = cl;
      cl = yyinput();
    }
    cool_yylval.error_msg = "String constant too long";
    return ERROR;
  }
}
 /* String contains escaped newline "a\
                                    b"
 */
<STRING>\\\n {
 if (append("\n")) {
    cool_yylval.error_msg = "String constant too long";
    return ERROR;
  }
  curr_lineno++;
}

 /* String contains unescaped newline "a
                                       b"
 */  
<STRING>\n {
  BEGIN INITIAL;
  curr_lineno++;
  cool_yylval.error_msg = "Unterminated string constant";
  return ERROR;
}
 /* String contains the un-escaped NULL char */
<STRING>\0|\\\0 {  
  BEGIN INITIAL;

  /* Head and loookahead char */
  int ch = 0, cl = yyinput(), res;
  while (true) {
    if (cl == EOF) {
      unput(cl);
      break;
    }
    if (IS_END_OF_STRING(ch, cl))
      break;
    ch = cl;
    cl = yyinput();
  }
  cool_yylval.error_msg = "String contains null character";
  return ERROR;
}
<STRING><<EOF>> {
  BEGIN INITIAL;
  cool_yylval.error_msg = "EOF in string constant";
  return ERROR;
}

 /* This rule comes after the one for escaped chars such as '\n' and companion:
    it transforms every string in the form "\c" in "c" as specified by the
    assignment instructions.
 */
<STRING>\\[^\\|\0] {
  char c[2] = { 0 };
  c[0] = yytext[1];
  if (append(c))
    return ERROR; 
}
<STRING>{STR_CONST} {
  BEGIN INITIAL;

  /* Empty string */
  if (string_buf_ptr == string_buf) {
    cool_yylval.symbol = stringtable.add_string("");
    return STR_CONST;
  }
  *string_buf_ptr = '\0';
  cool_yylval.symbol = stringtable.add_string(string_buf);
  return STR_CONST;
}
{INT_CONST} {
  cool_yylval.symbol = inttable.add_string(yytext);
  return INT_CONST;
}
{TYPEID} {
  cool_yylval.symbol = idtable.add_string(yytext);
  return TYPEID;
}
{OBJECTID} {
  cool_yylval.symbol = idtable.add_string(yytext);
  return OBJECTID;
}

{WHITE_SPACE} { ; }

 /*
  * Matches brackets, plus, minus ... more rule to allow error handling (TODO)
  */

[{};:]|"("|")"|"+"|"-"|"*"|"/"|"~"|"<"|"="|"."|","|"@" { return yytext[0]; }

. { 
  cool_yylval.error_msg = yytext;
  return ERROR;
}

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */


 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */


%%