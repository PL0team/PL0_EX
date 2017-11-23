#include <stdio.h>

#define NRW        16     // number of reserved words
#define TXMAX      500    // length of identifier table
#define MAXNUMLEN  14     // maximum number of digits in numbers
#define NSYM       13     // maximum number of symbols in array ssym and csym
#define MAXIDLEN   10     // length of identifiers

#define MAXADDRESS 32767  // maximum address
#define MAXLEVEL   32     // maximum depth of nesting block
#define CXMAX      500    // size of code array

#define MAXSYM     30     // maximum number of symbols
#define MAXDIM     10     // maximum dimension of arrays
#define MAXARR     30     // maxinum number of arrays
#define MAXPROC    30     // maxinum number of procedure
#define MAXARGC    10	  // maxinum number of procedure argument

#define STACKSIZE  1000   // maximum storage

#define TRUE 1
#define FALSE 0

enum symtype
{
	SYM_NULL,
	SYM_IDENTIFIER,
	SYM_NUMBER,
	SYM_INC,
	SYM_DEC,
	SYM_PLUS,
	SYM_MINUS,
	SYM_TIMES,
	SYM_SLASH,
	SYM_MOD,
	SYM_BIT_AND,
	SYM_BIT_OR,
	SYM_BIT_NOT,
	SYM_BIT_XOR,
	SYM_SHL,
	SYM_SHR,
	SYM_LOGIC_AND,
	SYM_LOGIC_OR,
	SYM_LOGIC_NOT,
	SYM_ODD,
	SYM_EQU,
	SYM_NEQ,
	SYM_LES,
	SYM_LEQ,
	SYM_GTR,
	SYM_GEQ,
	SYM_LPAREN,
	SYM_RPAREN,
	SYM_LINDEX,
	SYM_RINDEX,
	SYM_COMMA,
	SYM_SEMICOLON,
	SYM_QUES,
	SYM_COLON,
	SYM_PERIOD,
	SYM_ASSIGN,
	SYM_PLUS_ASSIGN,
	SYM_MINUS_ASSIGN,
	SYM_TIMES_ASSIGN,
	SYM_SLASH_ASSIGN,
	SYM_MOD_ASSIGN,
	SYM_BIT_AND_ASSIGN,
	SYM_BIT_OR_ASSIGN,
	SYM_BIT_XOR_ASSIGN,
	SYM_SHL_ASSIGN,
	SYM_SHR_ASSIGN,
  	SYM_BEGIN,
	SYM_END,
	SYM_IF,
	SYM_ELIF,
	SYM_ELSE,
	SYM_PRINT,
	SYM_FOR,
	SYM_WHILE,
	SYM_DO,
	SYM_BREAK,
	SYM_CONTINUE,
	SYM_CALL,
	SYM_CONST,
	SYM_VAR,
	SYM_PROCEDURE,
	SYM_RETURN,
	SYM_EXIT
};

enum idtype
{
	ID_CONSTANT, ID_VARIABLE, ID_PROCEDURE, ID_ARRAY
};

enum opcode
{
	LIT, OPR, LEA, LOD, STO, CAL, INT, JMP, JPC, JZ, JNZ, JE, JNE, JG, JGE, JL, JLE, JOD, RET, OUT, ALOD, ASTO
};

enum oprcode
{
	OPR_RET, OPR_NEG, OPR_ADD, OPR_MIN,
	OPR_MUL, OPR_DIV, OPR_MOD, OPR_ODD,
	OPR_EQU, OPR_NEQ, OPR_LES, OPR_LEQ,
	OPR_GTR, OPR_GEQ, OPR_SHL, OPR_SHR, OPR_BIT_NOT,
	OPR_LOGIC_NOT, OPR_LOGIC_AND, OPR_LOGIC_OR,
	OPR_BIT_AND, OPR_BIT_XOR, OPR_BIT_OR
};


typedef struct
{
	int f; // function code
	int l; // level
	int a; // displacement address
} instruction;

//////////////////////////////////////////////////////////////////////
char* err_msg[] =
{
/*  0 */    "",
/*  1 */    "Found '==' when expecting '='.",
/*  2 */    "There must be a number to follow '='.",
/*  3 */    "There must be an '=' to follow the identifier.",
/*  4 */    "There must be an identifier to follow 'const', 'var', or 'procedure'.",
/*  5 */    "Missing ',' or ';'.",
/*  6 */    "Incorrect procedure name.",
/*  7 */    "Statement expected.",
/*  8 */    "Follow the statement is an incorrect symbol.",
/*  9 */    "'$' expected.",
/* 10 */    "';' expected.",
/* 11 */    "Undeclared identifier.",
/* 12 */    "Illegal assignment.",
/* 13 */    "'=' expected.",
/* 14 */    "There must be an identifier to follow the 'call'.",
/* 15 */    "A constant or variable can not be called.",
/* 16 */    "Argument expected.",
/* 17 */    "';' or '}' expected.",
/* 18 */    "'do' expected.",
/* 19 */    "Incorrect symbol.",
/* 20 */    "Relative operators expected.",
/* 21 */    "Procedure identifier can not be in an expression.",
/* 22 */    "Missing ')'.",
/* 23 */    "The symbol can not be followed by a factor.",
/* 24 */    "The symbol can not be as the beginning of an expression.",
/* 25 */    "The number is too great.",
/* 26 */    "Missing '('.",
/* 27 */    "'{' expectes.",
/* 28 */    "'}' expected.",
/* 29 */    "'}' or '$' expected.",
/* 30 */    "Argc can't match.",
/* 31 */    "Number or const expected.",
/* 32 */    "There are too many levels.",
/* 33 */	"']' expected.",
/* 34 */ 	"The symbol can't be the type of a argument.",
/* 35 */	"'[' expected.",
/* 36 */	"Array dimension can't match.",
/* 37 */	"':' expected.",
/* 38 */	"Var expected.",
/* 39 */	"Incorrect use of \"continue\".",
/* 40 */	"Incorrect use of \"break\"."
};

//////////////////////////////////////////////////////////////////////
char ch;         // last character read
int  sym;        // last symbol read
char id[MAXIDLEN + 1]; // last identifier read
int  num;        // last number read
int  cc;         // character count
int  ll;         // line length
int  kk;
int  err;
int  cx;         // index of current instruction to be generated.
				 // next item num in code list, point to empty item
int  level = 0;
int  tx = 0;	 // last item num in symbol table, point to entered item
int  ax = 0;	 // last item num in array table, point to entered item
int  px = 0;	 // last item num in procedure table, point to entered item

char line[80];

instruction code[CXMAX];

char* word[NRW + 1] =
{
	"", /* place holder */
	"call", "const", "do", "if", "elif", "else", "print", 
	"odd", "procedure", "return", "exit", "var", "for", "while", "break", "continue"
};

int wsym[NRW + 1] =
{
	SYM_NULL,
	SYM_CALL, SYM_CONST, SYM_DO, SYM_IF, SYM_ELIF, SYM_ELSE, SYM_PRINT, 
	SYM_ODD, SYM_PROCEDURE, SYM_RETURN, SYM_EXIT, SYM_VAR, SYM_FOR, SYM_WHILE, SYM_BREAK, SYM_CONTINUE
};

int ssym[NSYM + 1] =
{
	SYM_NULL, SYM_BIT_NOT,
	SYM_LPAREN, SYM_RPAREN, SYM_LINDEX, SYM_RINDEX, SYM_EQU, SYM_COMMA, SYM_PERIOD, SYM_SEMICOLON,
	SYM_QUES, SYM_COLON, SYM_BEGIN, SYM_END
};

char csym[NSYM + 1] =
{
	' ', '~', '(', ')', '[', ']', '=', ',', '$', ';','?', ':', '{', '}'
};

#define MAXINS   22
char* mnemonic[MAXINS] =
{
	"LIT", "OPR", "LEA", "LOD", "STO", "CAL", "INT", "JMP", "JPC", "JZ", "JNZ", "JE", "JNE", "JG", "JGE", "JL", "JLE", "JOD", "RET", "OUT", "ALOD", "ASTO"
};

typedef struct
{
	char name[MAXIDLEN + 1];
	int  kind;
	int  value;
	int  index;
} comtab;

comtab table[TXMAX];

typedef struct
{
	char  name[MAXIDLEN + 1];
	int   kind;
	short level;
	short address;
	int   index;
} mask;

typedef struct
{
	int dimension;
	int up[MAXDIM];
	int count[MAXDIM];
} arrayinfo;

arrayinfo arraytable[MAXARR];

typedef struct
{
	int argc;
	int type[MAXARGC];
} procinfo;

procinfo proctable[MAXPROC];

FILE* infile;

// EOF PL0.h
