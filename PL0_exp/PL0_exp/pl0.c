// pl0 compiler source code

#pragma warning(disable:4996)


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "pl0.h"
#include "set.c"

//////////////////////////////////////////////////////////////////////
// print error message.
void error(int n)
{
	int i;

	printf("      ");
	for (i = 1; i <= cc - 1; i++)
		printf(" ");
	printf("^\n");
	printf("Error %3d: %s\n", n, err_msg[n]);
	err++;
} // error

//////////////////////////////////////////////////////////////////////
void getch(void)
{
	if (cc == ll)
	{
		if (feof(infile))
		{
			printf("\nPROGRAM INCOMPLETE\n");
			exit(1);
		}
		ll = cc = 0;
		printf("%5d  ", cx);
		while ( (!feof(infile)) // added & modified by alex 01-02-09
			    && ((ch = getc(infile)) != '\n'))
		{
			printf("%c", ch);
			line[++ll] = ch;
		} // while
		printf("\n");
		line[++ll] = ' ';
	}
	ch = line[++cc];
} // getch

int use_last_sym;		// used last read sym, used in assign_expr
int last_sym;			// last read sym, used in assign_expr

//////////////////////////////////////////////////////////////////////
// gets a symbol from input stream.
void getsym(void)
{
	int i, k;
	char a[MAXIDLEN + 1];

	if(use_last_sym == 1)
	{
		sym = last_sym;
		use_last_sym = 0;
	}
	else
	{
		while(TRUE)								// deal with space tab and comment
		{
			if(ch == ' ' || ch == '\t')			// space and tab
				getch();
			else if (ch == '/')
			{
				getch();
				if(ch == '*')					// /* comment */
				{
					do{
						do{
							getch();
						}while(ch != '*');
						getch();
					}while(ch != '/');
					getch();
				}
				else if(ch == '/')			// // comment
				{
					cc = ll;				// getch() doesn't return '\n', so I have to force getch()
											// to fetch char from next line by make cc = ll.
					getch();
				}
				else
				{
					ch = '/';				// others starts with '/'
					cc --;
					break;
				}
			}
			else
			{
				break;						// others
			}
		}// while

		if (isalpha(ch))
		{ // symbol is a reserved word or an identifier.
			k = 0;
			do
			{
				if (k < MAXIDLEN)
					a[k++] = ch;
				getch();
			}
			while (isalpha(ch) || isdigit(ch) || (ch == '_'));
			a[k] = 0;
			strcpy(id, a);
			word[0] = id;
			i = NRW;
			while (strcmp(id, word[i--]));
			if (++i)
				sym = wsym[i]; // symbol is a reserved word
			else
				sym = SYM_IDENTIFIER;   // symbol is an identifier
		}
		else if (isdigit(ch))
		{ // symbol is a number.
			k = num = 0;
			sym = SYM_NUMBER;
			do
			{
				num = num * 10 + ch - '0';
				k++;
				getch();
			}
			while (isdigit(ch));
			if (k > MAXNUMLEN)
				error(25);     // The number is too great.
		}
		else if (ch == '=')
		{
			getch();
			if (ch == '=')
			{
				sym = SYM_EQU; 		// ==
				getch();
			}
			else
			{
				sym = SYM_ASSIGN;     // =
			}
		}
		else if(ch == '&')
		{
			getch();
			if(ch == '&')		// &&
			{
				sym = SYM_LOGIC_AND;
				getch();
			}
			else		// &
			{
				sym = SYM_BIT_AND;
			}
		}
		else if(ch == '|')
		{
			getch();
			if(ch == '|')		// ||
			{
				sym = SYM_LOGIC_OR;
				getch();
			}
			else		// |
			{
				sym = SYM_BIT_OR;
			}
		}
		else if(ch == '!')
		{
			getch();
			if(ch == '=')		// !=
			{
				sym = SYM_NEQ;
				getch();
			}
			else		// !
			{
				sym = SYM_LOGIC_NOT;
			}
		}
		else if (ch == '>')
		{
			getch();
			if (ch == '=')
			{
				sym = SYM_GEQ;     // >=
				getch();
			}
			else
			{
				sym = SYM_GTR;     // >
			}
		}
		else if (ch == '<')
		{
			getch();
			if (ch == '=')
			{
				sym = SYM_LEQ;     // <=
				getch();
			}
			else
			{
				sym = SYM_LES;     // <
			}
		}
		else
		{ // other tokens
			i = NSYM;
			csym[0] = ch;
			while (csym[i--] != ch);
			if (++i)
			{
				sym = ssym[i];
				getch();
			}
			else
			{
				printf("Fatal Error: Unknown character.\n");
				exit(1);
			}
		}
	}
} // getsym

//////////////////////////////////////////////////////////////////////
// generates (assembles) an instruction.
void gen(int f, int l, int a)
{
	if (cx > CXMAX)
	{
		printf("Fatal Error: Program too long.\n");
		exit(1);
	}
	code[cx].f = f;
	code[cx].l = l;
	code[cx++].a = a;
} // gen

//////////////////////////////////////////////////////////////////////
// tests if error occurs and skips all symbols that do not belongs to s1 or s2.
// s1 is FIRST[A];
// test whether sym in s1, error when not
void test(symset s1, symset s2, int n)
{
	symset s;

	if (! inset(sym, s1))
	{
		error(n);
		s = uniteset(s1, s2);
		while(! inset(sym, s))
			getsym();
		destroyset(s);
	}
} // test

//////////////////////////////////////////////////////////////////////
int dx;  // data allocation index
		 // address of var in stack frame

// enter object(constant, variable or procedre) into table.
// a table of all the id(symbol table)
void enter(int kind)
{
	mask* mk;

	tx++;
	strcpy(table[tx].name, id);
	table[tx].kind = kind;
	switch (kind)
	{
	case ID_CONSTANT:
		if (num > MAXADDRESS)
		{
			error(25); // The number is too great.
			num = 0;
		}
		table[tx].value = num;
		break;
	case ID_VARIABLE:
		mk = (mask*) &table[tx];
		mk->level = level;
		mk->address = dx++;
		break;
	case ID_PROCEDURE:
		mk = (mask*) &table[tx];
		mk->level = level;
		break;
	case ID_ARGUMENT:
		mk = (mask*) &table[tx];
		mk->level = level;
		break;
	} // switch
} // enter

//////////////////////////////////////////////////////////////////////
// locates identifier in symbol table.
int position(char* id)
{
	// match from the end to the begin
	int i;
	strcpy(table[0].name, id); 	/* place holder */
	i = tx + 1;
	while (strcmp(table[--i].name, id) != 0);
	return i;
} // position

//////////////////////////////////////////////////////////////////////
void constdeclaration()
{
	if (sym == SYM_IDENTIFIER)
	{
		getsym();
		if (sym == SYM_EQU || sym == SYM_ASSIGN)
		{
			if (sym == SYM_EQU)
				error(1); // Found '==' when expecting '='.
			getsym();
			if (sym == SYM_NUMBER)
			{
				enter(ID_CONSTANT);
				getsym();
			}
			else
			{
				error(2); // There must be a number to follow '='.
			}
		}
		else
		{
			error(3); // There must be an '=' to follow the identifier.
		}
	}
	else
		error(4); // There must be an identifier to follow 'const', 'var', or 'procedure'.
} // constdeclaration

//////////////////////////////////////////////////////////////////////
void vardeclaration(void)
{
	if (sym == SYM_IDENTIFIER)
	{
		enter(ID_VARIABLE);
		getsym();
	}
	else
	{
		error(4); // There must be an identifier to follow 'const', 'var', or 'procedure'.
	}
} // vardeclaration

//////////////////////////////////////////////////////////////////////
void argdeclaration(void)
{
	if(sym == SYM_IDENTIFIER)
	{
		enter(ID_ARGUMENT);
		getsym();
	}
	else
	{
		error(16);	// argument expected.
	}
} // argdeclaration

//////////////////////////////////////////////////////////////////////
void args_decl(symset fsys)
{
	int count = 0;
	int procTx = tx;
	int arg_dx = -1;		// arg addr from -2 to the lower
	int index;
	mask *mk;
	if(sym == SYM_LPAREN)
	{
		getsym();
		if(sym != SYM_RPAREN)
		{
			argdeclaration();
			count += 1;
			while(sym == SYM_COMMA)
			{
				getsym();
				argdeclaration();
				count += 1;
			} // while
		} // if
		mk = (mask*)&table[procTx];  // proc in table
		mk->argc = count;
		for(index = count; index > 0; index --)
		{
			mk = (mask*)&table[procTx + index]; // index arg in table
			mk->address = arg_dx;
			arg_dx -= 1;
		}
		if(sym == SYM_RPAREN)
		{
			getsym();
		}
		else
		{
			error(22);	// Missing ')'.
		}
	} // if
	else
	{
		error(26);	// Missing '('.
	} // else
} // args_decl

//////////////////////////////////////////////////////////////////////
void listcode(int from, int to)
{
	int i;

	printf("\n");
	for (i = from; i < to; i++)
	{
		printf("%5d %s\t%d\t%d\n", i, mnemonic[code[i].f], code[i].l, code[i].a);
	}
	printf("\n");
} // listcode

//////////////////////////////////////////////////////////////////////
int args_list(symset fsys)
{
	void expression(symset fsys);
	symset set, set0;
	int count = 0;

	if(sym == SYM_LPAREN)
	{
		getsym();
		if(sym != SYM_RPAREN)
		{
			set0 = createset(SYM_COMMA, SYM_LPAREN, SYM_RPAREN, SYM_NULL);
			set = uniteset(fsys, set0);
			expression(set);
			count += 1;
			while(sym == SYM_COMMA)
			{
				getsym();
				expression(set);		// result will store on top of stack and become real argment
				count += 1;
			} // while
			destroyset(set);
			destroyset(set0);
		} // if
		if(sym == SYM_RPAREN)
			getsym();
		else
			error(22);		// Missing ')'.
	} // if
	else
	{
		error(26);		// Missing '('.
	} // else
	return count;
} // args_list

//////////////////////////////////////////////////////////////////////
void proc_call(symset fsys)
{
	symset set, set0;
	mask *mk;
	int count;
	int i;
	
	if(sym == SYM_IDENTIFIER && (i = position(id)))
	{
		mk = (mask*)&table[i];
		if(mk->kind == ID_PROCEDURE)
		{
			getsym();
			if(sym == SYM_LPAREN)
			{
				gen(INT, 0, 1);								// room for return value
				set0 = createset(SYM_LPAREN, SYM_RPAREN, SYM_NULL);
				set = uniteset(fsys, set0);
				count = args_list(set);
				destroyset(set);
				destroyset(set0);
				gen(CAL, level - mk->level, mk->address);	// build new stack frame
				if(count != mk->argc)
				{
					error(30);		// Argc can't match.
				}
			} // if
			else
			{
				error(26);		// Missing '('.
			} // else
		} // if
	} // if
} // proc_call

//////////////////////////////////////////////////////////////////////
void factor(symset fsys)
{
	void expression(symset fsys);
	int i;
	symset set, set0;

	test(facbegsys, fsys, 24); // The symbol can not be as the beginning of an expression.

	while (inset(sym, facbegsys))
	{
		if (sym == SYM_IDENTIFIER)
		{
			if ((i = position(id)) == 0)
			{
				error(11); // Undeclared identifier.
			}
			else
			{
				switch (table[i].kind)
				{
					mask* mk;
					case ID_CONSTANT:
						gen(LIT, 0, table[i].value);
						getsym();
						break;
					case ID_VARIABLE:
						mk = (mask*) &table[i];
						gen(LOD, level - mk->level, mk->address);
						getsym();
						break;
					case ID_ARGUMENT:
						mk = (mask*)&table[i];
						gen(LOD, level - mk->level, mk->address);
						getsym();
						break;
					case ID_PROCEDURE:
						proc_call(fsys);
						break;
				} // switch
			}
		}
		else if (sym == SYM_NUMBER)
		{
			if (num > MAXADDRESS)
			{
				error(25); // The number is too great.
				num = 0;
			}
			gen(LIT, 0, num);
			getsym();
		}
		else if (sym == SYM_LPAREN)
		{
			getsym();
			set0 = createset(SYM_RPAREN, SYM_NULL);
			set = uniteset(set0, fsys);
			expression(set);
			destroyset(set);
			destroyset(set0);
			if (sym == SYM_RPAREN)
			{
				getsym();
			}
			else
			{
				error(22); // Missing ')'.
			}
		}
		else if(sym == SYM_MINUS) // UMINUS, Expr -> '-' Expr
		{
			 getsym();
			 expression(fsys);
			 gen(OPR, 0, OPR_NEG);
		}
		else if (sym == SYM_LOGIC_NOT) // LOGIC_NOT, Expr -> '!' Expr
		{
			getsym();
			expression(fsys);
			gen(OPR, 0, OPR_LOGIC_NOT);
		}
		else if (sym == SYM_BIT_NOT) // BIT_NOT, Expr -> '~' Expr
		{
			getsym();
			expression(fsys);
			gen(OPR, 0, OPR_BIT_NOT);
		}
		set0 = createset(SYM_LPAREN, SYM_NULL);
		test(fsys, set0, 23);
		destroyset(set0);
	} // while
} // factor

//////////////////////////////////////////////////////////////////////
void term(symset fsys)
{
	int mulop;
	symset set, set0;

	set0 = createset(SYM_TIMES, SYM_SLASH, SYM_MOD, SYM_NULL);
	set = uniteset(fsys, set0);
	factor(set);
	while (sym == SYM_TIMES || sym == SYM_SLASH || sym == SYM_MOD)
	{
		mulop = sym;
		getsym();
		factor(set);
		if (mulop == SYM_TIMES)
		{
			gen(OPR, 0, OPR_MUL);
		}
		else if (mulop == SYM_SLASH)
		{
			gen(OPR, 0, OPR_DIV);
		}
		else
		{
			gen(OPR, 0, OPR_MOD);
		}
	} // while
	destroyset(set);
	destroyset(set0);
} // term

//////////////////////////////////////////////////////////////////////
void polyn(symset fsys)
{
	int addop;
	symset set, set0;

	set0 = createset(SYM_PLUS, SYM_MINUS, SYM_NULL);
	set = uniteset(fsys, set0);

	term(set);
	while (sym == SYM_PLUS || sym == SYM_MINUS)
	{
		addop = sym;
		getsym();
		term(set);
		if (addop == SYM_PLUS)
		{
			gen(OPR, 0, OPR_ADD);
		}
		else
		{
			gen(OPR, 0, OPR_MIN);
		}
	} // while

	destroyset(set);
	destroyset(set0);
} // polyn

//////////////////////////////////////////////////////////////////////
void rel_expr(symset fsys)
{
	int relop;
	symset set;

	if (sym == SYM_ODD)
	{
		getsym();
		polyn(fsys);
		gen(OPR, 0, OPR_ODD);
	}
	else
	{
		set = uniteset(relset, fsys);
		polyn(set);
		destroyset(set);
		if (inset(sym, relset))
		{
			relop = sym;
			getsym();
			polyn(fsys);
			switch (relop)
			{
			case SYM_EQU:
				gen(OPR, 0, OPR_EQU);
				break;
			case SYM_NEQ:
				gen(OPR, 0, OPR_NEQ);
				break;
			case SYM_LES:
				gen(OPR, 0, OPR_LES);
				break;
			case SYM_GEQ:
				gen(OPR, 0, OPR_GEQ);
				break;
			case SYM_GTR:
				gen(OPR, 0, OPR_GTR);
				break;
			case SYM_LEQ:
				gen(OPR, 0, OPR_LEQ);
				break;
			} // switch
		} // else
	} // else
} // rel_expr

//////////////////////////////////////////////////////////////////////
void bit_and_expr(symset fsys)
{
	symset set, set0;
	set0 = createset(SYM_BIT_AND, SYM_NULL);
	set = uniteset(fsys, set0);
	rel_expr(set);
	while(sym == SYM_BIT_AND)
	{
		getsym();
		rel_expr(set);
		gen(OPR, 0, OPR_BIT_AND);
	} // while
	destroyset(set);
	destroyset(set0);
} // bit_and_expr

//////////////////////////////////////////////////////////////////////
void bit_xor_expr(symset fsys)
{
	symset set, set0;
	set0 = createset(SYM_BIT_XOR, SYM_NULL);
	set = uniteset(fsys, set0);
	bit_and_expr(set);
	while(sym == SYM_BIT_XOR)
	{
		getsym();
		bit_and_expr(set);
		gen(OPR, 0, OPR_BIT_XOR);
	} // while
	destroyset(set);
	destroyset(set0);
} // bit_xor_expr

//////////////////////////////////////////////////////////////////////
void bit_or_expr(symset fsys)
{
	symset set, set0;
	set0 = createset(SYM_BIT_OR, SYM_NULL);
	set = uniteset(fsys, set0);
	bit_xor_expr(set);
	while(sym == SYM_BIT_OR)
	{
		getsym();
		bit_xor_expr(set);
		gen(OPR, 0, OPR_BIT_OR);
	} // while
	destroyset(set);
	destroyset(set0);
} // bit_or_expr

//////////////////////////////////////////////////////////////////////
void logic_and_expr(symset fsys)
{
	symset set, set0;
	set0 = createset(SYM_LOGIC_AND, SYM_NULL);
	set = uniteset(fsys, set0);
	bit_or_expr(set);
	while(sym == SYM_LOGIC_AND)
	{
		getsym();
		bit_or_expr(set);
		gen(OPR, 0, OPR_LOGIC_AND);
	} // while
	destroyset(set);
	destroyset(set0);
} // logic_and_expr

//////////////////////////////////////////////////////////////////////
void logic_or_expr(symset fsys)
{
	symset set, set0;
	set0 = createset(SYM_LOGIC_OR, SYM_NULL);
	set = uniteset(fsys, set0);
	logic_and_expr(set);
	while(sym == SYM_LOGIC_OR)
	{
		getsym();
		logic_and_expr(set);
		gen(OPR, 0, OPR_LOGIC_OR);
	} // while
	destroyset(set);
	destroyset(set0);
} // logic_or_expr

//////////////////////////////////////////////////////////////////////
void condition_assign_expr(symset fsys)
{
	// Todo
	logic_or_expr(fsys);
} // condition_assign_expr

//////////////////////////////////////////////////////////////////////
void assign_expr(symset fsys)
{
	void expression(symset fsys);
	int i;
	symset set0, set;

	if(sym == SYM_IDENTIFIER)
	{
		mask* mk;
		if (! (i = position(id)))
		{
			error(11); // Undeclared identifier.
		} // if
		else if(table[i].kind == ID_VARIABLE || table[i].kind == ID_ARGUMENT)
		{
			last_sym = sym;
			getsym();
			if (sym == SYM_ASSIGN)
			{
				getsym();
				set0 = createset(SYM_ASSIGN, SYM_NULL);
				set = uniteset(fsys, set0);
				expression(set);
				destroyset(set);
				destroyset(set0);
				mk = (mask*) &table[i];
				if (i)
				{
					gen(STO, level - mk->level, mk->address);
				}
			} // if
			else
			{
				int temp = last_sym;
				last_sym = sym;
				sym = temp;
				use_last_sym = 1;
				condition_assign_expr(fsys);
			} // else
		} // else if
		else
		{
			condition_assign_expr(fsys);
		} // else
	} // if
	else
	{
		condition_assign_expr(fsys);
	}
} // assign_expr

//////////////////////////////////////////////////////////////////////
void expression(symset fsys)
{
	assign_expr(fsys);
} // expression

//////////////////////////////////////////////////////////////////////
void condition(symset fsys)
{
	symset set, set0;
	if (sym != SYM_LPAREN)
	{
		error(26);			// Missing '('
	} // if
	else
	{
		getsym();
		set0 = createset(SYM_RPAREN, SYM_NULL);
		set = uniteset(fsys, set0);
		expression(set);
		destroyset(set);
		destroyset(set0);
		if (sym != SYM_RPAREN)
		{
			error(22);		// Missing ')'.
		} // if
		getsym();
	} // else
} // condition

int curr_proc;		// the procedure which return statement belongs to

//////////////////////////////////////////////////////////////////////
void statement(symset fsys)
{
	void complex_statement(symset fsys);
	int i, cx1, cx2;
	int count;
	symset set1, set;

	if (sym == SYM_IDENTIFIER)
	{ // assign statement
		assign_expr(fsys);
		gen(INT, 0, -1);		// recycle stack room storing assign value
		if(sym == SYM_SEMICOLON)
			getsym();
		else
			error(10);			// ';' expected.
	}
	else if(sym == SYM_PRINT)
	{ // print statement
		getsym();
		count = args_list(fsys);
		gen(OUT, 0, count);
		if(sym == SYM_SEMICOLON)
			getsym();
		else
			error(10);			// ';' expected.
	}
	else if(sym == SYM_RETURN)
	{ // return statement
		getsym();
		if(sym != SYM_SEMICOLON)
			expression(fsys);		// return value
		else
			gen(LIT, 0, 0);		// return 0 in default
		mask *mk;
		if(curr_proc)
		{
			mk = (mask*)&table[curr_proc];
			gen(RET, 0, mk->argc);
		}
		else
		{
			gen(RET, 0, 0);
		}
		if(sym == SYM_SEMICOLON)
			getsym();
		else
			error(10);			// ';' expected.
	}
	else if (sym == SYM_IF)
	{ // if statement
		getsym();
		set1 = createset(SYM_DO, SYM_NULL);
		set = uniteset(set1, fsys);
		condition(set);
		destroyset(set1);
		destroyset(set);
		cx1 = cx;
		gen(JPC, 0, 0);
		complex_statement(fsys);
		code[cx1].a = cx;
	}
	else if (sym == SYM_WHILE)
	{ // while statement
		cx1 = cx;
		getsym();
		set1 = createset(SYM_DO, SYM_NULL);
		set = uniteset(set1, fsys);
		condition(set);
		destroyset(set1);
		destroyset(set);
		cx2 = cx;
		gen(JPC, 0, 0);
		complex_statement(fsys);
		gen(JMP, 0, cx1);
		code[cx2].a = cx;
	}
	else if (sym == SYM_BEGIN)
	{ // complex_statement
		complex_statement(fsys);
	}
	test(fsys, phi, 19);
} // statement

//////////////////////////////////////////////////////////////////////
void multi_statement(symset fsys)
{
	symset set;
	set = uniteset(fsys, statbegsys);
	while(inset(sym, statbegsys))
	{
		statement(set);
	}
	if(sym != SYM_PERIOD && sym != SYM_END)
		error(29);				// '}' or '$' expected.
	destroyset(set);
} //  multi_statement

//////////////////////////////////////////////////////////////////////
void complex_statement(symset fsys)
{
	symset set0, set;
	set0 = createset(SYM_BEGIN, SYM_END, SYM_NULL);
	set = uniteset(fsys, set0);
	if(sym == SYM_BEGIN)
	{
		getsym();
		multi_statement(set);
		if(sym == SYM_END)
			getsym();
		else
			error(28);		// '}' expected.
	}
	else
	{
		statement(set);
	}
	destroyset(set);
	destroyset(set0);
} //  complex_statement

//////////////////////////////////////////////////////////////////////
void block(symset fsys)
{
	int cx0; // initial code index
	mask* mk;
	int block_dx;
	int savedTx;
	int savedProc;
	symset set1, set;

	dx = 3;						// init dx
	block_dx = dx;
	mk = (mask*) &table[curr_proc];
	mk->address = cx;			// procedure entry, will be modified later
								// code[cx] point to (JMP, 0, X) in the following line
	gen(JMP, 0, 0);				// jump to procedure entry, 3rd arg will be modified later
	if (level > MAXLEVEL)
	{
		error(32); // There are too many levels.
	}
	do
	{
		if (sym == SYM_CONST)
		{ // constant declarations
			getsym();
			constdeclaration();
			while (sym == SYM_COMMA)
			{
				getsym();
				constdeclaration();
			}
			if (sym == SYM_SEMICOLON)
			{
				getsym();
			}
			else
			{
				error(5); // Missing ',' or ';'.
			}
		} // if
		else if (sym == SYM_VAR)
		{ // variable declarations
			getsym();
			vardeclaration();
			while (sym == SYM_COMMA)
			{
				getsym();
				vardeclaration();
			}
			if (sym == SYM_SEMICOLON)
			{
				getsym();
			}
			else
			{
				error(5); // Missing ',' or ';'.
			}
		} // if
		else if(sym == SYM_PROCEDURE)
		{ // procedure declarations
			block_dx = dx; // save dx before handling procedure call!
						   // dx will be different in inside block
			getsym();
			if (sym == SYM_IDENTIFIER)
			{
				enter(ID_PROCEDURE);
				getsym();
			}
			else
			{
				error(4); // There must be an identifier to follow 'const', 'var', or 'procedure'.
			}

			level++;
			savedTx = tx;	// save tx before analyse inside block
			savedProc = curr_proc;
			curr_proc = tx;

			args_decl(fsys);
			if (sym == SYM_BEGIN)
			{
				getsym();
			}
			else
			{
				error(27); //'{' expectes.
			}
			set1 = createset(SYM_SEMICOLON, SYM_BEGIN, SYM_END, SYM_NULL);
			set = uniteset(set1, fsys);
			block(set);
			destroyset(set1);
			destroyset(set);
			if(sym == SYM_END)
			{
				getsym();
			}
			else
			{
				error(28);	// '}' expected.
			}

			tx = savedTx;	// restore tx after analyse inside block to remove all local var from sym table
							// to prevent outer visiting inside var
			curr_proc = savedProc;
			level--;
			dx = block_dx; //restore dx after handling procedure call!
		} // if	
	} // do
	while (inset(sym, declbegsys));
	test(statbegsys, declbegsys, 7);

	code[mk->address].a = cx;		// modify procedure entry and JMP code
	mk->address = cx;
	cx0 = cx;
	gen(INT, 0, block_dx);
	set1 = createset(SYM_SEMICOLON, SYM_END, SYM_NULL);
	set = uniteset(set1, fsys);
	multi_statement(set);
	destroyset(set1);
	destroyset(set);
	gen(LIT, 0, 0);		// return 0 in default
	if(curr_proc)		// default return statement in case there is no return statement in block
		gen(RET, 0, mk->argc);
	else
		gen(RET, 0, 0);
	test(fsys, phi, 8); // test for error: Follow the statement is an incorrect symbol.
	listcode(cx0, cx);
} // block

//////////////////////////////////////////////////////////////////////
int base(int stack[], int currentLevel, int levelDiff)
{
	int b = currentLevel;

	while (levelDiff--)
		b = stack[b];
	return b;
} // base

//////////////////////////////////////////////////////////////////////
// interprets and executes codes.
void interpret()
{
	int pc;        // program counter
	int stack[STACKSIZE];
	int top;       // top of stack
	int b;         // program, base, and top-stack register
	int j;
	instruction i; // instruction register

	printf("Begin executing PL/0 program.\n");

	pc = 0;
	b = 1;
	top = 3;
	stack[1] = stack[2] = stack[3] = 0;
	do
	{
		i = code[pc++];
		switch (i.f)
		{
		case LIT:
			stack[++top] = i.a;
			break;
		case OPR:
			switch (i.a) // operator
			{
			case OPR_RET:
				top = b - 1;
				pc = stack[top + 3];
				b = stack[top + 2];
				break;
			case OPR_NEG:
				stack[top] = -stack[top];
				break;
			case OPR_BIT_NOT:
			 	stack[top] = ~stack[top];
				break;
			case OPR_LOGIC_NOT:
				stack[top] = !stack[top];
				break;
			case OPR_ADD:
				top--;
				stack[top] += stack[top + 1];
				break;
			case OPR_MIN:
				top--;
				stack[top] -= stack[top + 1];
				break;
			case OPR_MUL:
				top--;
				stack[top] *= stack[top + 1];
				break;
			case OPR_DIV:
				top--;
				if (stack[top + 1] == 0)
				{
					fprintf(stderr, "Runtime Error: Divided by zero.\n");
					fprintf(stderr, "Program terminated.\n");
					continue;
				}
				stack[top] /= stack[top + 1];
				break;
			case OPR_MOD:
				top --;
				if( stack[top + 1] == 0)
				{
					fprintf(stderr, "Runtime Error: Divided by zero.\n");
					fprintf(stderr, "Program terminated.\n");
					continue;
				}
				stack[top] %= stack[top + 1];
				break;
			case OPR_ODD:
				stack[top] = (stack[top] % 2 == 1);
				break;
			case OPR_EQU:
				top--;
				stack[top] = stack[top] == stack[top + 1];
				break;
			case OPR_NEQ:
				top--;
				stack[top] = stack[top] != stack[top + 1];
				break;
			case OPR_LES:
				top--;
				stack[top] = stack[top] < stack[top + 1];
				break;
			case OPR_GEQ:
				top--;
				stack[top] = stack[top] >= stack[top + 1];
				break;
			case OPR_GTR:
				top--;
				stack[top] = stack[top] > stack[top + 1];
				break;
			case OPR_LEQ:
				top--;
				stack[top] = stack[top] <= stack[top + 1];
				break;
			case OPR_BIT_AND:
				top --;
			 	stack[top] = stack[top] & stack[top + 1];
			 	break;
			case OPR_BIT_XOR:
				top --;
			 	stack[top] = stack[top] ^ stack[top + 1];
				break;
			case OPR_BIT_OR:
				top --;
				stack[top] = stack[top] | stack[top + 1];
				break;
			case OPR_LOGIC_AND:
				top --;
				stack[top] = stack[top] && stack[top + 1];
				break;
			case OPR_LOGIC_OR:
				top --;
				stack[top] = stack[top] || stack[top + 1];
				break;
			} // switch
			break;
		case LOD:
			stack[++top] = stack[base(stack, b, i.l) + i.a];
			break;
		case STO:
			stack[base(stack, b, i.l) + i.a] = stack[top];
			// printf("%d\n", stack[top]);
			// top--;		// reserve value for assign expr
			break;
		case CAL:
			stack[top + 1] = base(stack, b, i.l);
			// generate new block mark
			stack[top + 2] = b;
			stack[top + 3] = pc;
			b = top + 1;
			pc = i.a;
			break;
		case INT:
			top += i.a;
			break;
		case JMP:
			pc = i.a;
			break;
		case JPC:
			if (stack[top] == 0)
				pc = i.a;
			top--;
			break;
		case OUT:
			for(j = top - i.a + 1; j <= top; j ++)
				printf("%d\t", stack[j]);
			printf("\n");
			top -= i.a;
			break;
		case RET:
			stack[b - i.a - 1] = stack[top];
			top = b - i.a - 1;
			pc = stack[b + 2];
			b = stack[b + 1];
			break;
		} // switch
	}
	while (pc);

	printf("End executing PL/0 program.\n");
} // interpret

//////////////////////////////////////////////////////////////////////
int main (int argc, char **argv)
{
	FILE* hbin;
	char s[80];
	int i;
	symset set, set1, set2;

	if(argc != 2)
	{
		printf("Usage: pl0.exe path_to_src_file\n");
	}
	else{
		strcpy(s, argv[1]);						// get file name to be compiled
		if ((infile = fopen(s, "r")) == NULL)
		{
			printf("File %s can't be opened.\n", s);
			exit(1);
		}

		phi = createset(SYM_NULL);
		relset = createset(SYM_EQU, SYM_NEQ, SYM_LES, SYM_LEQ, SYM_GTR, SYM_GEQ, SYM_NULL);

		// create begin symbol sets
		declbegsys = createset(SYM_CONST, SYM_VAR, SYM_PROCEDURE, SYM_NULL);
		statbegsys = createset(SYM_IDENTIFIER, SYM_PRINT, SYM_RETURN, SYM_IF, SYM_WHILE, SYM_BEGIN, SYM_NULL);
		facbegsys = createset(SYM_IDENTIFIER, SYM_NUMBER, SYM_LPAREN, SYM_MINUS, SYM_LOGIC_NOT, SYM_BIT_NOT, SYM_NULL);

		err = cc = cx = ll = 0; // initialize global variables
		ch = ' ';
		kk = MAXIDLEN;
		level = 0;
		tx = 0;
		use_last_sym = 0;
		curr_proc = 0;

		getsym();

		set1 = createset(SYM_PERIOD, SYM_NULL);
		set2 = uniteset(declbegsys, statbegsys);
		set = uniteset(set1, set2);
		block(set);
		destroyset(set1);
		destroyset(set2);
		destroyset(set);
		destroyset(phi);
		destroyset(relset);
		destroyset(declbegsys);
		destroyset(statbegsys);
		destroyset(facbegsys);
		if (sym != SYM_PERIOD)
			error(9); // '$' expected.
		if (err == 0)
		{
			hbin = fopen("hbin.txt", "w");
			for (i = 0; i < cx; i++)
				fwrite(&code[i], sizeof(instruction), 1, hbin);
			fclose(hbin);
		}
		if (err == 0)
			interpret();
		else
			printf("There are %d error(s) in PL/0 program.\n", err);
		listcode(0, cx);
	}
	return 0;
} // main

//////////////////////////////////////////////////////////////////////
// eof pl0.c
