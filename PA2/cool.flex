/*
 *  The scanner definition for COOL.
 */

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

static int comment_level = 0;
static int invalid_string = 0;

#define APPEND_CHAR(c) \
	if ( string_buf_ptr >= &string_buf[MAX_STR_CONST - 1] ) { \
		if ( invalid_string == 0 ) { \
			invalid_string = 1; \
			yylval.error_msg = "String constant too long"; \
			return ERROR; \
		} \
	} \
	else { \
		if ( invalid_string == 0 ) { \
			*string_buf_ptr = c; \
			string_buf_ptr++; \
		} \
	}

/*
    Tokens. 
	#define CLASS 258
	#define ELSE 259
	#define FI 260
	#define IF 261
	#define IN 262
	#define INHERITS 263
	#define LET 264
	#define LOOP 265
	#define POOL 266
	#define THEN 267
	#define WHILE 268
	#define CASE 269
	#define ESAC 270
	#define OF 271
	#define DARROW 272
	#define NEW 273
	#define ISVOID 274
	#define STR_CONST 275
	#define INT_CONST 276
	#define BOOL_CONST 277
	#define TYPEID 278
	#define OBJECTID 279
	#define ASSIGN 280
	#define NOT 281
	#define LE 282
#define ERROR 283	// Note, you should ignore the token called error [in lowercase] for this assignment; it is used by the parser in PA3.)
#define LET_STMT 285 // You should ignore the token LET STMT. It is used only by the parser (PA3).
 */

%}

/*
 * Define names for regular expressions here.
 */

DARROW          =>
ASSIGN          <-
LE              <=
DIGIT           [0-9]
%Start          COMMENT 
%Start          INLINE_COMMENT
%Start          STRING 


%%

 /*
  *  Nested comments
  */
<INITIAL,COMMENT>"(*" {
	comment_level++;
	BEGIN(COMMENT);
} 

<COMMENT>{
	"*)" {
		comment_level--;
		if (comment_level == 0)
			BEGIN(INITIAL);
	}
	<<EOF>> {
		BEGIN(INITIAL);
		yylval.error_msg = "EOF in comment";
		return ERROR;
	}
	\n { curr_lineno++; }
	. {}	/* Skip every char in the comment */
}

<INITIAL>"*)" {
	yylval.error_msg = "Unmatched *)";
	return ERROR;
}

<INITIAL>"--" {
	BEGIN(INLINE_COMMENT);
}

<INLINE_COMMENT>\n {
	curr_lineno++;
	BEGIN(INITIAL);
}

<INLINE_COMMENT>. {} /* Skip every char in the inline comment */



 /*
  *  The multiple-character operators.
  */
<INITIAL>{
	{DARROW}		{ return DARROW; }
	{ASSIGN}		{ return ASSIGN;}
	{LE}			{ return LE; }
}

 /*
  *  The single-character operators.
  */
<INITIAL>[\+\-\*/\.,;(){}:@=~<] { return yytext[0]; }

 /*
  * Keywords are case-insensitive except for the values true and false,
  * which must begin with a lower-case letter.
  */

<INITIAL>{
	(?i:class)	{ return CLASS; }
	(?i:else)		{ return ELSE; }
	(?i:fi)		{ return FI; }
	(?i:if)		{ return IF; }
	(?i:in)		{ return IN; }
	(?i:inherits)	{ return INHERITS; }
	(?i:let)		{ return LET; }
	(?i:loop)		{ return LOOP; }
	(?i:pool)		{ return POOL; }
	(?i:then)		{ return THEN; }
	(?i:while)	{ return WHILE; }
	(?i:case)		{ return CASE; }
	(?i:esac)		{ return ESAC; }
	(?i:of)		{ return OF; }
	(?i:new)		{ return NEW; }
	(?i:isvoid)	{ return ISVOID; }
	(?i:not)		{ return NOT; }

	t(?i:rue) {
		cool_yylval.boolean = 1;
		return BOOL_CONST;
	}
	f(?i:alse) {
		cool_yylval.boolean = 0;
		return BOOL_CONST;
	}
}

 /*
  *  Integers and identifiers
  */

<INITIAL>{DIGIT}+ {
	cool_yylval.symbol = inttable.add_string(yytext);
	return INT_CONST;
}

<INITIAL>[A-Z][A-Za-z0-9_]* {
	cool_yylval.symbol = idtable.add_string(yytext);
	return TYPEID;
}

<INITIAL>[a-z][A-Za-z0-9_]* {
	cool_yylval.symbol = idtable.add_string(yytext);
	return OBJECTID;
}

 /*
  *  String constants (C syntax)
  *  Escape sequence \c is accepted for all characters c. Except for 
  *  \n \t \b \f, the result is c.
  *
  */
<INITIAL>\" {
	string_buf_ptr = string_buf;
	invalid_string = 0;
	BEGIN(STRING);
}

<STRING>{
	\" {
		*string_buf_ptr = '\0'; 
		BEGIN(INITIAL);
		if (invalid_string == 0){
			cool_yylval.symbol = stringtable.add_string(string_buf);
			return STR_CONST;
		}
	}
	<<EOF>> {
		BEGIN(INITIAL);
		yylval.error_msg = "EOF in string constant";
		return ERROR;
	}
	\0 {
		invalid_string = 1;
		yylval.error_msg = "String contains null character";
		return ERROR;
	}
	\\\0 {
		invalid_string = 1;
		yylval.error_msg = "String contains escaped null character";
		return ERROR;
	}
	\n {
		BEGIN(INITIAL);
		curr_lineno++;
		if (invalid_string == 0) {
			yylval.error_msg = "Unterminated string constant";
			return ERROR;
		}
	}
	\\b { APPEND_CHAR('\b'); }
	\\t { APPEND_CHAR('\t'); }
	\\n { APPEND_CHAR('\n'); }
	\\f { APPEND_CHAR('\f'); }
	\\\\ { APPEND_CHAR('\\'); }
	\\\n {
		curr_lineno++;
		APPEND_CHAR('\n');
	}
	\\. { APPEND_CHAR(yytext[1]); }
	.	{ APPEND_CHAR(yytext[0]); }
}


 /*
  *  White spaces and invalid char errors 
  */

<INITIAL>\n { curr_lineno++;}

<INITIAL>[ \f\r\t\v] {}

<INITIAL>. {
	yylval.error_msg = yytext;
	return ERROR;
}

%%
