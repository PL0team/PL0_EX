// pl0 compiler source code

#pragma warning(disable:4996)


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "pl0.h"
#include "set.c"

//////////////////////////////////////////////////////////////////////
int dx; 	 			// data allocation index
						// address of var in stack frame
//int id_read_ahead;		// whether id already processed, used in assign_expr
int curr_proc;			// the procedure which return statement belongs to

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

//////////////////////////////////////////////////////////////////////
// gets a symbol from input stream.
void getsym(void)
{
	int i, k;
	char a[MAXIDLEN + 1];

	while(TRUE)								// deal with space tab and comment
	{
		if(ch == ' ' || ch == '\t' || ch == '\r')			// space and tab
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
		}while (isalpha(ch) || isdigit(ch) || (ch == '_'));
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
	else if(ch == '+')
	{
		getch();
		if(ch == '+')	// ++
		{
			sym = SYM_INC;
			getch();
		}
		else if(ch == '=')	// +=
		{
			sym = SYM_PLUS_ASSIGN;
			getch();
		}
		else		// +
		{
			sym = SYM_PLUS;
		}
	}
	else if(ch == '-')
	{
		getch();
		if(ch == '-')	// --
		{
			sym = SYM_DEC;
			getch();
		}
		else if(ch == '=')	// -=
		{
			sym = SYM_MINUS_ASSIGN;
			getch();
		}
		else	// -
		{
			sym = SYM_MINUS;
		}
	}
	else if(ch == '*')
	{
		getch();
		if(ch == '=')	// *=
		{
			sym = SYM_TIMES_ASSIGN;
			getch();
		}
		else	// *
		{
			sym = SYM_TIMES;
		}
	}
	else if(ch == '/')
	{
		getch();
		if(ch == '=')	// /=
		{
			sym = SYM_SLASH_ASSIGN;
			getch();
		}
		else	// /
		{
			sym = SYM_SLASH;
		}
	}
	else if(ch == '%')
	{
		getch();
		if(ch == '=')	// %=
		{
			sym = SYM_MOD_ASSIGN;
			getch();
		}
		else		// %
		{
			sym = SYM_MOD;
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
		else if(ch == '=')	// &=
		{
			sym = SYM_BIT_AND_ASSIGN;
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
		else if(ch == '=')	// |=
		{
			sym = SYM_BIT_OR_ASSIGN;
			getch();
		}
		else		// |
		{
			sym = SYM_BIT_OR;
		}
	}
	else if(ch == '^')
	{
		getch();
		if(ch == '=')	// ^=
		{
			sym = SYM_BIT_XOR_ASSIGN;
			getch();
		}
		else	// ^
		{
			sym = SYM_BIT_XOR;
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
		if(ch == '>')
		{
			getch();
			if(ch == '=')	// >>=
			{
				sym = SYM_SHR_ASSIGN;
				getch();
			}
			else	// >>
			{
				sym = SYM_SHR;
			}
		}
		else if (ch == '=')
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
		if(ch == '<')
		{
			getch();
			if(ch == '=')	// <<=
			{
				sym  = SYM_SHL_ASSIGN;
				getch();
			}
			else		// <<
			{
				sym = SYM_SHL;
			}
		}
		else if (ch == '=')
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
		px ++;
		mk->index = px;
		break;
	case ID_ARRAY:
		mk = (mask*) &table[tx];
		mk->level = level;
		mk->address = dx;
		ax ++;
		mk->index = ax;
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
void backpatch(codelist l, int num)
{
	snode *p;
	p = l->next;
	while(p != NULL)
	{
		l->next = p->next;
		code[p->elem].a = num - (p->elem);
		p->elem = -1000000;
		free(p);
		p = l->next;
	}

} // backpatch

//////////////////////////////////////////////////////////////////////
void logic2num(codelist truelist, codelist falselist)
{
	if(! isempty(truelist))
	{
		backpatch(truelist, cx);
		gen(LIT, 0, 1);
		gen(JMP, 0, 2);
		backpatch(falselist, cx);
		gen(LIT, 0, 0);
	}
} // logic2num

//////////////////////////////////////////////////////////////////////
void num2logic(codelist truelist, codelist falselist)
{
	if(isempty(truelist))
	{
		insertlist(falselist, cx);
		gen(JZ, 0, 0);
		insertlist(truelist, cx);
		gen(JMP, 0, 0);
	}
} // num2logic

//////////////////////////////////////////////////////////////////////
// by default, conditional jump code looks like:JX true/JMP false
// After reverse, it will become: JNX false(by default it will goto true)
void conditionreverse(codelist truelist, codelist falselist)
{
	symset conjmpset;
	conjmpset = createset(JZ, JNZ, JE, JNE, JG, JGE, JL, JLE, SYM_NULL);
	if(code[cx - 1].f == JMP && inlist(cx - 1, truelist))
	{
		deletelist(truelist, cx - 1);
		cx --;
	}
	if(code[cx - 1].f == JMP && inlist(cx - 1, falselist) && inset(code[cx -2].f, conjmpset) && inlist(cx - 2, truelist))
	{
		if(code[cx- 2].f == JZ)
			code[cx - 2].f = JNZ;
		else if(code[cx- 2].f == JNZ)
			code[cx - 2].f = JZ;
		else if(code[cx- 2].f == JE)
			code[cx - 2].f = JNE;
		else if(code[cx- 2].f == JNE)
			code[cx - 2].f = JE;
		else if(code[cx- 2].f == JG)
			code[cx - 2].f = JLE;
		else if(code[cx- 2].f == JLE)
			code[cx - 2].f = JG;
		else if(code[cx- 2].f == JL)
			code[cx - 2].f = JGE;
		else if(code[cx- 2].f == JGE)
			code[cx - 2].f = JL;
		deletelist(falselist, cx - 1);
		insertlist(falselist,cx - 2);
		cx --;
	}
	destroyset(conjmpset);
} // conditionreverse

//////////////////////////////////////////////////////////////////////
void listcode(int from, int to)
{
	int i;

	printf("\n");
	for (i = from; i < to; i++)
		printf("%5d %s\t%d\t%d\n", i, mnemonic[code[i].f], code[i].l, code[i].a);
	printf("\n");
} // listcode

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
	int dim;
	int i;
	if (sym == SYM_IDENTIFIER)
	{
		getsym();
		if(sym != SYM_LINDEX)
		{
			enter(ID_VARIABLE);
		} // if
		else
		{
			enter(ID_ARRAY);
			dim = 0;
			while(sym == SYM_LINDEX)
			{
				getsym();
				if(sym == SYM_NUMBER)
				{
					arraytable[ax].up[dim] = num - 1;
					dim += 1;
				}
				else if(sym == SYM_IDENTIFIER)
				{
					if((i = position(id)) == 0)
					{
						error(11); // Undeclared identifier.
						getsym();
					} // if
					else
					{
						if(table[i].kind == ID_CONSTANT)
						{
							arraytable[ax].up[dim] = table[i].value - 1;
							dim += 1;
						}
						else
						{
							error(31);			// Number or const expected.
							getsym();
						}
					}
				}
				else
				{
					error(31);		// Number or const expected.
					getsym();
				}
				getsym();
				if(sym == SYM_RINDEX)
					getsym();
				else
					error(33);		// ']' expected.
			} // while
			arraytable[ax].dimension = dim;
			int i;
			arraytable[ax].count[dim - 1] = 1;
			for(i = dim - 2; i >= 0; i --)
				arraytable[ax].count[i] = arraytable[ax].count[i + 1] * (arraytable[ax].up[i + 1] + 1);
			dx += arraytable[ax].count[0] * (arraytable[ax].up[0] + 1);
		} // else
	} // if
	else
	{
		error(4); // There must be an identifier to follow 'const', 'var', or 'procedure'.
	} // else
} // vardeclaration

//////////////////////////////////////////////////////////////////////
int argdeclaration(void)
{
	if(sym == SYM_VAR)
	{
		getsym();
		vardeclaration();
	}
	else
	{
		error(34);		// The symbol can't be the type of a argument.
	}
	return table[tx].kind;
} // argdeclaration

//////////////////////////////////////////////////////////////////////
void args_decl(symset fsys)
{
	int count = 0;
	int procTx = tx;
	int arg_max = -1;		// arg addr from -1 to the lower
	int index;
	mask *mk;
	dx = 0;
	if(sym == SYM_LPAREN)
	{
		getsym();
		if(sym != SYM_RPAREN)
		{
			proctable[px].type[count] = argdeclaration();
			count += 1;
			while(sym == SYM_COMMA)
			{
				getsym();
				argdeclaration();
				proctable[px].type[count] = table[tx].kind;
				count += 1;
			} // while
		} // if
		proctable[px].argc = count;
		for(index = count; index > 0; index --)
		{
			mk = (mask*)&table[procTx + index]; // index arg in table
			mk->address = mk->address - dx + arg_max + 1;
		}
		dx = arg_max + 1;
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
int args_list(symset fsys)
{
	void expression(symset fsys, codelist truelist, codelist falselist);
	symset set, set0;
	int count = 0;

	if(sym == SYM_LPAREN)
	{
		getsym();
		if(sym != SYM_RPAREN)
		{
			set0 = createset(SYM_COMMA, SYM_LPAREN, SYM_RPAREN, SYM_NULL);
			set = uniteset(fsys, set0);
			expression(set, NULL, NULL);
			count += 1;
			while(sym == SYM_COMMA)
			{
				getsym();
				expression(set, NULL, NULL);
				// result will store on top of stack and become real argment
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
void proc_call(symset fsys, int i)
{
	symset set, set0;
	mask *mk = (mask*) &table[i];
	int count;

	gen(INT, 0, 1);
	getsym();
	set0 = createset(SYM_LPAREN, SYM_RPAREN, SYM_NULL);
	set = uniteset(fsys, set0);
	count = args_list(set);
	destroyset(set);
	destroyset(set0);
	gen(CAL, level - mk->level, mk->address);	// build new stack frame
	gen(INT, 0, - count);		// pop all args
	if(count != proctable[mk->index].argc)
	{
		error(30);		// Argc can't match.
	}
} // proc_call

//////////////////////////////////////////////////////////////////////
void array_index(symset fsys, int i)
{
	void expression(symset fsys, codelist truelist, codelist falselist);
	int count;
	int index = ((mask*) &table[i])->index;
	symset set0, set;
	set0 = createset(SYM_LINDEX, SYM_RINDEX, SYM_NULL);
	set = uniteset(fsys, set0);
	count = 0;
	if(sym == SYM_LINDEX)
	{
		getsym();
		expression(set, NULL, NULL);
		gen(LIT, 0, arraytable[index].count[count]);
		gen(OPR, 0, OPR_MUL);
		count += 1;
		if(sym == SYM_RINDEX)
			getsym();
		else
			error(33);		// ']' expected.
		while(sym == SYM_LINDEX && count < arraytable[index].dimension)
		{
			getsym();
			expression(set, NULL, NULL);
			gen(LIT, 0, arraytable[index].count[count]);
			gen(OPR, 0, OPR_MUL);
			gen(OPR, 0, OPR_ADD);
			count += 1;
			if(sym == SYM_RINDEX)
				getsym();
			else
				error(33);		// ']' expected.
		}
		if(count != arraytable[index].dimension)
			error(36);			// Array dimension can't match.
	}
	else
	{
		error(35);		// '[' expected.
	}
	destroyset(set);
	destroyset(set0);
} // array_index

//////////////////////////////////////////////////////////////////////
void factor(symset fsys, codelist truelist, codelist falselist)
{
	void expression(symset fsys, codelist truelist, codelist falselist);
	int i;
	symset set, set0;
	codelist sub_truelist, sub_falselist;

	test(facbegsys, fsys, 24); // The symbol can not be as the beginning of an expression.

	if (inset(sym, facbegsys))
	{
		if (sym == SYM_IDENTIFIER)
		{
			if ((i = position(id)) == 0)
			{
				error(11); // Undeclared identifier.
				getsym();
			} // if
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
						if(sym == SYM_INC || sym == SYM_DEC)
						{
							gen(LOD, level - mk->level, mk->address);
							gen(LIT, 0, 1);
							if(sym == SYM_INC)
								gen(OPR, 0, OPR_ADD);
							else
								gen(OPR, 0, OPR_MIN);
							gen(STO, level - mk->level, mk->address);
							gen(INT, 0, -1);
							getsym();
						}
						break;
					case ID_ARRAY:
						mk = (mask*) &table[i];
						gen(LEA, level - mk->level, mk->address);
						getsym();
						array_index(fsys, i);
						gen(OPR, 0, OPR_ADD);
						gen(ALOD, 0, 0);
						break;
					case ID_PROCEDURE:
						proc_call(fsys, i);
						break;
				} // switch
			} // else
		} // if
		else if(sym == SYM_INC || sym == SYM_DEC)
		{
			int opr = ((sym == SYM_INC)? OPR_ADD : OPR_MIN);
			mask *mk;
			getsym();
			if(sym == SYM_IDENTIFIER)
			{
				if((i = position(id)) != 0)
				{
					if(table[i].kind == ID_VARIABLE)
					{
						mk = (mask*) &table[i];
						gen(LOD, level - mk->level, mk->address);
						gen(LIT, 0, 1);
						gen(OPR, 0, opr);
						gen(STO, level - mk->level, mk->address);
						getsym();
					}
					else
					{
						error(38);		// Var expected.
						getsym();
					}
				}
				else
				{
					error(11); 		// Undeclared identifier.
					getsym();
				}
			}
			else
			{
				error(38);		// Var expected.
				getsym();
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
		} // if
		else if (sym == SYM_LPAREN)
		{
			getsym();
			set0 = createset(SYM_RPAREN, SYM_NULL);
			set = uniteset(set0, fsys);
			expression(set, truelist, falselist);
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
		} // if
		else if(sym == SYM_MINUS) // UMINUS, Expr -> '-' Expr
		{
			 getsym();
			 factor(fsys, truelist, falselist);
			 logic2num(truelist, falselist);
			 gen(OPR, 0, OPR_NEG);
		} // if
		else if (sym == SYM_LOGIC_NOT) // LOGIC_NOT, Expr -> '!' Expr
		{
			sub_truelist = createlist();
			sub_falselist = createlist();
			getsym();
			factor(fsys, sub_truelist, sub_falselist);
			num2logic(sub_truelist, sub_falselist);
			unitelist(truelist, sub_falselist);
			unitelist(falselist, sub_truelist);
			destroylist(sub_truelist);
			destroylist(sub_falselist);
			//gen(OPR, 0, OPR_LOGIC_NOT);
		} // if
		else if (sym == SYM_BIT_NOT) // BIT_NOT, Expr -> '~' Expr
		{
			getsym();
			factor(fsys, truelist, falselist);
			logic2num(truelist, falselist);
			gen(OPR, 0, OPR_BIT_NOT);
		} // if
		else if (sym == SYM_RAND)
		{
			getsym();
			factor(fsys, truelist, falselist);
			logic2num(truelist, falselist);
			gen(RAND, 0, 0);
		}
		set0 = createset(SYM_LPAREN, SYM_NULL);
		test(fsys, set0, 23);
		destroyset(set0);
	} // if
} // factor

//////////////////////////////////////////////////////////////////////
void term(symset fsys, codelist truelist, codelist falselist)
{
	int mulop;
	symset set, set0;

	set0 = createset(SYM_TIMES, SYM_SLASH, SYM_MOD, SYM_NULL);
	set = uniteset(fsys, set0);
	factor(set, truelist, falselist);
	if(sym == SYM_TIMES || sym == SYM_SLASH || sym == SYM_MOD)
		logic2num(truelist, falselist);
	while (sym == SYM_TIMES || sym == SYM_SLASH || sym == SYM_MOD)
	{
		mulop = sym;
		getsym();
		factor(set, truelist, falselist);
		logic2num(truelist, falselist);
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
void polyn(symset fsys, codelist truelist, codelist falselist)
{
	int addop;
	symset set, set0;

	set0 = createset(SYM_PLUS, SYM_MINUS, SYM_NULL);
	set = uniteset(fsys, set0);

	term(set, truelist, falselist);
	if(sym == SYM_PLUS || sym == SYM_MINUS)
		logic2num(truelist, falselist);
	while (sym == SYM_PLUS || sym == SYM_MINUS)
	{
		addop = sym;
		getsym();
		term(set, truelist, falselist);
		logic2num(truelist, falselist);
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
void shift_expr(symset fsys, codelist truelist, codelist falselist)
{
	int shiftop;
	symset set0, set;

	set0 = createset(SYM_SHL, SYM_SHR, SYM_NULL);
	set = uniteset(fsys, set0);

	polyn(set, truelist, falselist);
	if(sym == SYM_SHL || sym == SYM_SHR)
		logic2num(truelist, falselist);
	while(sym == SYM_SHL || sym == SYM_SHR)
	{
		shiftop = sym;
		getsym();
		polyn(set, truelist, falselist);
		logic2num(truelist, falselist);
		if(shiftop == SYM_SHL)
			gen(OPR, 0, OPR_SHL);
		else
			gen(OPR, 0, OPR_SHR);
	} // while

	destroyset(set);
	destroyset(set0);
} // shift_expr

//////////////////////////////////////////////////////////////////////
void rel_expr(symset fsys, codelist truelist, codelist falselist)
{
	int relop;
	symset set;

	if (sym == SYM_ODD)
	{
		getsym();
		shift_expr(fsys, truelist, falselist);
		logic2num(truelist, falselist);
		//gen(OPR, 0, OPR_ODD);
		insertlist(truelist, cx);
		gen(JOD, 0, 0);
		insertlist(falselist, cx);
		gen(JMP, 0, 0);
	}
	else
	{
		set = uniteset(relset, fsys);
		shift_expr(set, truelist, falselist);
		while (inset(sym, relset))
		{
			logic2num(truelist, falselist);
			relop = sym;
			getsym();
			shift_expr(set, truelist, falselist);
			logic2num(truelist, falselist);
			switch (relop)
			{
			case SYM_EQU:
				//gen(OPR, 0, OPR_EQU);
				insertlist(truelist, cx);
				gen(JE, 0, 0);
				insertlist(falselist, cx);
				gen(JMP, 0, 0);
				break;
			case SYM_NEQ:
				//gen(OPR, 0, OPR_NEQ);
				insertlist(truelist, cx);
				gen(JNE, 0, 0);
				insertlist(falselist, cx);
				gen(JMP, 0, 0);
				break;
			case SYM_LES:
				//gen(OPR, 0, OPR_LES);
				insertlist(truelist, cx);
				gen(JL, 0, 0);
				insertlist(falselist, cx);
				gen(JMP, 0, 0);
				break;
			case SYM_GEQ:
				//gen(OPR, 0, OPR_GEQ);
				insertlist(truelist, cx);
				gen(JGE, 0, 0);
				insertlist(falselist, cx);
				gen(JMP, 0, 0);
				break;
			case SYM_GTR:
				//gen(OPR, 0, OPR_GTR);
				insertlist(truelist, cx);
				gen(JG, 0, 0);
				insertlist(falselist, cx);
				gen(JMP, 0, 0);
				break;
			case SYM_LEQ:
				//gen(OPR, 0, OPR_LEQ);
				insertlist(truelist, cx);
				gen(JLE, 0, 0);
				insertlist(falselist, cx);
				gen(JMP, 0, 0);
				break;
			} // switch
		} // while
		destroyset(set);
	} // else
} // rel_expr

//////////////////////////////////////////////////////////////////////
void bit_and_expr(symset fsys, codelist truelist, codelist falselist)
{
	symset set, set0;
	set0 = createset(SYM_BIT_AND, SYM_NULL);
	set = uniteset(fsys, set0);
	rel_expr(set, truelist, falselist);
	if(sym == SYM_BIT_AND)
		logic2num(truelist, falselist);
	while(sym == SYM_BIT_AND)
	{
		getsym();
		rel_expr(set, truelist, falselist);
		logic2num(truelist, falselist);
		gen(OPR, 0, OPR_BIT_AND);
	} // while
	destroyset(set);
	destroyset(set0);
} // bit_and_expr

//////////////////////////////////////////////////////////////////////
void bit_xor_expr(symset fsys, codelist truelist, codelist falselist)
{
	symset set, set0;
	set0 = createset(SYM_BIT_XOR, SYM_NULL);
	set = uniteset(fsys, set0);
	bit_and_expr(set, truelist, falselist);
	if(sym == SYM_BIT_XOR)
		logic2num(truelist, falselist);
	while(sym == SYM_BIT_XOR)
	{
		getsym();
		bit_and_expr(set, truelist, falselist);
		logic2num(truelist, falselist);
		gen(OPR, 0, OPR_BIT_XOR);
	} // while
	destroyset(set);
	destroyset(set0);
} // bit_xor_expr

//////////////////////////////////////////////////////////////////////
void bit_or_expr(symset fsys, codelist truelist, codelist falselist)
{
	symset set, set0;
	set0 = createset(SYM_BIT_OR, SYM_NULL);
	set = uniteset(fsys, set0);
	bit_xor_expr(set, truelist, falselist);
	if(sym == SYM_BIT_OR)
		logic2num(truelist, falselist);
	while(sym == SYM_BIT_OR)
	{
		getsym();
		bit_xor_expr(set, truelist, falselist);
		logic2num(truelist, falselist);
		gen(OPR, 0, OPR_BIT_OR);
	} // while
	destroyset(set);
	destroyset(set0);
} // bit_or_expr

//////////////////////////////////////////////////////////////////////
void logic_and_expr(symset fsys, codelist truelist, codelist falselist)
{
	codelist bit_truelist, bit_falselist;
	symset set, set0;
	bit_truelist = createlist();
	bit_falselist = createlist();
	set0 = createset(SYM_LOGIC_AND, SYM_NULL);
	set = uniteset(fsys, set0);
	bit_or_expr(set, bit_truelist, bit_falselist);
	if(sym == SYM_LOGIC_AND)
		num2logic(bit_truelist, bit_falselist);
	while(sym == SYM_LOGIC_AND)
	{
		// Unnecessary jmp delete
		if(code[cx -1].f == JMP && inlist(cx - 1, bit_truelist))
		{
			deletelist(bit_truelist, cx - 1);
			cx --;
		}
		// condition reverse
		conditionreverse(bit_truelist, bit_falselist);
		unitelist(falselist, bit_falselist);
		backpatch(bit_truelist, cx);
		getsym();
		bit_or_expr(set, bit_truelist, bit_falselist);
		num2logic(bit_truelist, bit_falselist);
		//gen(OPR, 0, OPR_LOGIC_AND);
	} // while
	unitelist(falselist, bit_falselist);
	unitelist(truelist, bit_truelist);
	destroylist(bit_truelist);
	destroylist(bit_falselist);
	destroyset(set);
	destroyset(set0);
} // logic_and_expr

//////////////////////////////////////////////////////////////////////
void logic_or_expr(symset fsys, codelist truelist, codelist falselist)
{
	codelist and_truelist,and_falselist;
	symset set, set0;
	and_truelist = createlist();
	and_falselist = createlist();
	set0 = createset(SYM_LOGIC_OR, SYM_NULL);
	set = uniteset(fsys, set0);
	logic_and_expr(set, and_truelist, and_falselist);
	if(sym == SYM_LOGIC_OR)
		num2logic(and_truelist, and_falselist);
	while(sym == SYM_LOGIC_OR)
	{
		getsym();
		unitelist(truelist, and_truelist);
		if(code[cx - 1].f == JMP && inlist(cx - 1, and_falselist))
		{
			deletelist(and_falselist, cx - 1);
			cx --;
		}
		backpatch(and_falselist, cx);
		logic_and_expr(set, and_truelist, and_falselist);
		num2logic(and_truelist, and_falselist);
		//gen(OPR, 0, OPR_LOGIC_OR);
	} // while
	unitelist(truelist, and_truelist);
	unitelist(falselist, and_falselist);
	destroylist(and_truelist);
	destroylist(and_falselist);
	destroyset(set);
	destroyset(set0);
} // logic_or_expr

//////////////////////////////////////////////////////////////////////
void condition_assign_expr(symset fsys, codelist truelist, codelist falselist)
{
	codelist or_truelist, or_falselist;
	symset set, set0;
	int cx0;
	or_truelist = createlist();
	or_falselist = createlist();
	set0 = createset(SYM_QUES, SYM_COLON, SYM_NULL);
	set = uniteset(fsys, set0);
	logic_or_expr(set, or_truelist, or_falselist);
	if(sym == SYM_QUES)
	{
		num2logic(or_truelist, or_falselist);
		backpatch(or_truelist, cx);
		if(code[cx - 1].f == JMP && code[cx - 1].a == 1)
			cx --;
		getsym();
		condition_assign_expr(set, NULL, NULL);
		cx0 = cx;
		gen(JMP, 0, 0);
		if(sym == SYM_COLON)
		{
			backpatch(or_falselist, cx);
			getsym();
			condition_assign_expr(set, NULL, NULL);
			code[cx0].a = cx;
		}
		else
		{
			error(37);		// ':' expected.
		}
		if(truelist != NULL)
		{
			unitelist(truelist, or_truelist);
			unitelist(falselist, or_falselist);
		}
	}
	else
	{
		if(truelist == NULL)
		{
			logic2num(or_truelist, or_falselist);
		}
		else
		{
			unitelist(truelist, or_truelist);
			unitelist(falselist, or_falselist);
		}
	}
	destroyset(set);
	destroyset(set0);
	destroylist(or_truelist);
	destroylist(or_falselist);
} // condition_assign_expr

//////////////////////////////////////////////////////////////////////
void assign_expr(symset fsys, codelist truelist, codelist falselist)
{
	void expression(symset fsys, codelist truelist, codelist falselist);
	int cx0, f, a, l;
	int opr;
	//int i;
	symset set0, set;
	codelist sub_truelist, sub_falselist;

	sub_truelist = createlist();
	sub_falselist = createlist();
	set0 = createset(SYM_ASSIGN, SYM_PLUS_ASSIGN, SYM_MINUS_ASSIGN, SYM_TIMES_ASSIGN, SYM_SLASH_ASSIGN, SYM_MOD_ASSIGN, SYM_BIT_AND_ASSIGN, SYM_BIT_OR_ASSIGN, SYM_BIT_XOR_ASSIGN, SYM_SHL_ASSIGN, SYM_SHR_ASSIGN, SYM_NULL);
	set = uniteset(fsys, set0);
	condition_assign_expr(set, sub_truelist, sub_falselist);
	cx0 = cx - 1;
	f = code[cx0].f;
	l = code[cx0].l;
	a = code[cx0].a;
	if(f == LOD || f == ALOD)
	// left-value check for assign_expr
	{
		if(sym == SYM_ASSIGN)
		{
			cx --;
			getsym();
			assign_expr(fsys, NULL, NULL);
			if(f == LOD)
				gen(STO, l, a);
			else
				gen(ASTO, l, a);
		}
		else
		{
			if(inset(sym, set0))
			{
				opr = sym;
				getsym();
				assign_expr(fsys, NULL, NULL);
				if(opr == SYM_PLUS_ASSIGN)
					gen(OPR, 0, OPR_ADD);
				else if(opr == SYM_MINUS_ASSIGN)
					gen(OPR, 0, OPR_MIN);
				else if(opr == SYM_TIMES_ASSIGN)
					gen(OPR, 0, OPR_MUL);
				else if(opr == SYM_SLASH_ASSIGN)
					gen(OPR, 0, OPR_DIV);
				else if(opr == SYM_MOD_ASSIGN)
					gen(OPR, 0, OPR_MOD);
				else if(opr == SYM_BIT_AND_ASSIGN)
					gen(OPR, 0, OPR_BIT_AND);
				else if(opr == SYM_BIT_OR_ASSIGN)
					gen(OPR, 0, OPR_BIT_OR);
				else if(opr == SYM_BIT_XOR_ASSIGN)
					gen(OPR, 0, OPR_BIT_XOR);
				else if(opr == SYM_SHL_ASSIGN)
					gen(OPR, 0, OPR_SHL);
				else
					gen(OPR, 0, OPR_SHR);
				if(f == LOD)
					gen(STO, l, a);
				else
					gen(ASTO, l, a);
			}
		}
	}
	if(truelist != NULL)
	{
		unitelist(truelist, sub_truelist);
		unitelist(falselist, sub_falselist);
	}
	else
	{
		logic2num(sub_truelist, sub_falselist);
	}
	destroyset(set);
	destroyset(set0);
	destroylist(sub_truelist);
	destroylist(sub_falselist);
} // assign_expr

//////////////////////////////////////////////////////////////////////
void expression(symset fsys, codelist truelist, codelist falselist)
{
	codelist ass_truelist, ass_falselist;
	ass_truelist = createlist();
	ass_falselist = createlist();
	assign_expr(fsys, ass_truelist, ass_falselist);
	if(truelist == NULL)
	{
		logic2num(ass_truelist, ass_falselist);
	}
	else
	{
		//num2logic(ass_truelist, ass_falselist);
		unitelist(truelist, ass_truelist);
		unitelist(falselist, ass_falselist);
	}
	destroylist(ass_truelist);
	destroylist(ass_falselist);
} // expression

//////////////////////////////////////////////////////////////////////
void condition(symset fsys, codelist truelist, codelist falselist)
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
		expression(set, truelist, falselist);
		num2logic(truelist, falselist);
		destroyset(set);
		destroyset(set0);
		if (sym != SYM_RPAREN)
		{
			error(22);		// Missing ')'.
		} // if
		getsym();
	} // else
} // condition

//////////////////////////////////////////////////////////////////////
void statement(symset fsys, codelist nextlist, codelist looplist)
{
	void multi_statement(symset fsys, codelist nextlist, codelist looplist);
	int cx0;
	int count;
	symset set0, set;
	symset set1, set2;
	codelist truelist, falselist;

	set0 = createset(SYM_IF, SYM_ELIF, SYM_ELSE, SYM_WHILE, SYM_DO, SYM_FOR, SYM_NULL);
	set = uniteset(fsys, set0);
	if (sym == SYM_IDENTIFIER || sym == SYM_INC || sym == SYM_DEC)
	{ // expression statement
		expression(fsys, NULL, NULL);
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
	// else if(sym == SYM_RAND)
	// {// rand statement
	// 	getsym();
	// 	count = args_list(fsys);
	// 	if(count != 1) error(41);
	// 	gen(RAND, 0, count);
	// 	if(sym == SYM_SEMICOLON) getsym();
	// 	else error(10);
	// }
	else if(sym == SYM_RETURN)
	{ // return statement
		mask *mk;
		int retval_addr;
		if(curr_proc)
		{
			mk = (mask*)&table[curr_proc];
			retval_addr = - proctable[mk->index].argc - 1;
		}
		else
		{
			retval_addr = -1;
		}
		getsym();
		if(sym != SYM_SEMICOLON)
			expression(fsys, NULL, NULL);		// return value
		else
			gen(LIT, 0, 0);		// default return value 0 in case there is no explicit return value
		gen(STO, 0, retval_addr);	// store return value
		gen(RET, 0, 0);			// clear stack frame
		if(sym == SYM_SEMICOLON)
			getsym();
		else
			error(10);			// ';' expected.
	}
	else if(sym == SYM_EXIT)
	{
		gen(JMP, 0, - cx);
		getsym();
		if(sym == SYM_SEMICOLON)
			getsym();
		else
			error(10);	// ';' expected.
	}
	else if(sym == SYM_CONTINUE)
	{
		if(looplist != NULL)
		{
			insertlist(looplist, cx);
			gen(JMP, 0, 0);
		}
		else
			error(39);		// Incorrect use of "continue".
		getsym();
		if(sym == SYM_SEMICOLON)
			getsym();
		else
			error(10);			// ';' expected.
	}
	else if(sym == SYM_BREAK)
	{
		if(nextlist != NULL)
		{
			insertlist(nextlist, cx);
			gen(JMP, 0, 0);
		}
		else
			error(40);		// Incorrect use of "break".
		getsym();
		if(sym == SYM_SEMICOLON)
			getsym();
		else
			error(10);			// ';' expected.
	}
	else if (sym == SYM_IF)
	{ // if statement
		codelist if_nextlist;
		truelist = createlist();
		falselist = createlist();
		if_nextlist = createlist();
		getsym();
		condition(fsys, truelist, falselist);
		conditionreverse(truelist, falselist);
		//cx1 = cx;
		//gen(JPC, 0, 0);
		backpatch(truelist, cx);
		statement(set, nextlist, looplist);
		insertlist(if_nextlist, cx);
		gen(JMP, 0, 0);
		while(sym == SYM_ELIF)
		{
			backpatch(falselist, cx);
			getsym();
			condition(fsys, truelist, falselist);
			conditionreverse(truelist, falselist);
			backpatch(truelist, cx);
			statement(set, nextlist, looplist);
			insertlist(if_nextlist, cx);
			gen(JMP, 0, 0);
		}
		if(sym == SYM_ELSE)
		{
			//cx0 = cx;
			//gen(JMP, 0, 0);
			backpatch(falselist, cx);
			getsym();
			statement(set, nextlist, looplist);
			backpatch(if_nextlist, cx);
			//code[cx0].a = cx;
		}
		else
		{
			if(code[cx - 1].f == JMP && inlist(cx - 1, if_nextlist))
			{
				deletelist(if_nextlist, cx - 1);
				cx --;
			}
			backpatch(falselist, cx);
			backpatch(if_nextlist, cx);
		}
		//code[cx1].a = cx;
		destroylist(truelist);
		destroylist(falselist);
		destroylist(if_nextlist);
	}
	else if (sym == SYM_WHILE)
	{ // while statement
		codelist sub_nextlist, sub_looplist;
		truelist = createlist();
		falselist = createlist();
		sub_nextlist = createlist();
		sub_looplist = createlist();
		cx0 = cx;
		getsym();
		condition(fsys, truelist, falselist);
		conditionreverse(truelist, falselist);
		//cx2 = cx;
		//gen(JPC, 0, 0);
		backpatch(truelist, cx);
		statement(set, sub_nextlist, sub_looplist);
		gen(JMP, 0, cx0 - cx);
		backpatch(sub_looplist, cx0);
		backpatch(sub_nextlist, cx);
		//code[cx2].a = cx;
		backpatch(falselist, cx);
		destroylist(truelist);
		destroylist(falselist);
		destroylist(sub_nextlist);
		destroylist(sub_looplist);
	}
	else if(sym == SYM_FOR)
	{ // for statement
		codelist sub_nextlist, sub_looplist;
		int start, end, con;
		int i;
		truelist = createlist();
		falselist = createlist();
		sub_nextlist = createlist();
		sub_looplist = createlist();
		set1 = createset(SYM_LPAREN, SYM_RPAREN, SYM_COMMA, SYM_SEMICOLON, SYM_NULL);
		set2 = uniteset(fsys, set1);
		getsym();
		if(sym == SYM_LPAREN)
		{
			do
			{
				getsym();
				expression(set2, NULL, NULL);
				gen(INT, 0, -1);
			}
			while(sym == SYM_COMMA);
			if(sym == SYM_SEMICOLON)
			{
				con = cx;
				getsym();
				expression(set2, truelist, falselist);
				num2logic(truelist, falselist);
				conditionreverse(truelist, falselist);
				if(sym == SYM_SEMICOLON)
				{
					start = cx;
					do
					{
						getsym();
						expression(set2, NULL, NULL);
						gen(INT, 0, -1);
					}
					while(sym == SYM_COMMA);
					end = cx - 1;
					if(sym == SYM_RPAREN)
					{
						getsym();
						statement(set, sub_nextlist, sub_looplist);
						// move code segment
						backpatch(sub_looplist, cx);
						// keep relative position between continue and start of increasement
						for(i = start; i <= end; cx ++, i ++)
							code[cx] = code[i];
						// keep relative position between break and next code after (JMP condition)
						// (JMP condition) will be in cx later after moving codes
						backpatch(sub_nextlist, cx + 1);
						cx0 = cx - 1;
						for(cx = start, i = end + 1; i <= cx0; cx ++, i ++)
							code[cx] = code[i];
						backpatch(truelist, start);
						// keep absolute position between (JMP condition) and condition(won't be moved)
						// JMP generated after moving
						gen(JMP, 0, con - cx);
						backpatch(falselist, cx);
					}
					else
						error(22);		// Missing ')'.
				}
				else
					error(10);		//';' expected.
			}
			else
				error(10);		//';' expected.
		}
		else
			error(26);		// Missing '('.
		destroyset(set1);
		destroyset(set2);
		destroylist(truelist);
		destroylist(falselist);
		destroylist(sub_nextlist);
		destroylist(sub_looplist);
	}
	else if (sym == SYM_BEGIN)
	{ // compound_statement
		set1 = createset(SYM_BEGIN, SYM_END, SYM_NULL);
		set2 = uniteset(fsys, set0);
		getsym();
		multi_statement(set, nextlist, looplist);
		if(sym == SYM_END)
			getsym();
		else
			error(28);		// '}' expected.
		destroyset(set1);
		destroyset(set2);
	}
	destroyset(set);
	destroyset(set0);
	test(fsys, phi, 19);
} // statement

//////////////////////////////////////////////////////////////////////
void multi_statement(symset fsys, codelist nextlist, codelist looplist)
{
	symset set;
	set = uniteset(fsys, statbegsys);
	while(inset(sym, statbegsys))
	{
		statement(set, nextlist, looplist);
	}
	if(sym != SYM_PERIOD && sym != SYM_END)
		error(29);				// '}' or '$' expected.
	destroyset(set);
} //  multi_statement

//////////////////////////////////////////////////////////////////////
void block(symset fsys)
{
	int cx0; // initial code index
	mask* mk;
	int block_dx;
	int savedTx;
	int savedAx;
	int savedPx;
	int savedProc;
	int retval_addr;
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
			savedAx = ax;
			savedPx = px;
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
			ax = savedAx;
			px = savedPx;
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
	gen(INT, 0, dx);
	set1 = createset(SYM_SEMICOLON, SYM_END, SYM_NULL);
	set = uniteset(set1, fsys);
	multi_statement(set, NULL, NULL);
	destroyset(set1);
	destroyset(set);
	gen(LIT, 0, 0);		// return 0 in default
	if(curr_proc)
		retval_addr = - proctable[mk->index].argc - 1;
	else
		retval_addr = -1;
	// default return statement in case there is no return statement in block
	gen(STO, 0, retval_addr);
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
void overflow(int top)
{
	if(top >= STACKSIZE)
	{
		fprintf(stderr, "Runtime Error: Stack Overflow.\n");
		fprintf(stderr, "Program terminated.\n");
		exit(0);
	}
} //overflow

//////////////////////////////////////////////////////////////////////
// interprets and executes codes.
void interpret()
{
	int pc;        // program counter
	int stack[STACKSIZE];
	int top;       // top of stack
	int b;         // program, base, and top-stack register
	int j;
	int addr;
	time_t temp; // temp arg
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
			overflow(top);
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
			case OPR_SHL:
				top --;
				if (stack[top + 1] > 31)
				{
					fprintf(stderr, "Runtime Error: Shift bits too much.\n");
					fprintf(stderr, "Program terminated.\n");
					continue;
				}
				stack[top] = stack[top] << stack[top + 1];
				break;
			case OPR_SHR:
				top --;
				if (stack[top + 1] > 31)
				{
					fprintf(stderr, "Runtime Error: Shift bits too much.\n");
					fprintf(stderr, "Program terminated.\n");
					continue;
				}
				stack[top] = stack[top] >> stack[top + 1];
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
		case LEA:
			stack[++top] = base(stack, b, i.l) + i.a;
			overflow(top);
			break;
		case LOD:
			stack[++top] = stack[base(stack, b, i.l) + i.a];
			overflow(top);
			break;
		case STO:
			stack[base(stack, b, i.l) + i.a] = stack[top];
			// printf("%d\n", stack[top]);
			// reserve value for assign expr
			// top--;
			break;
		case ALOD:
			addr = stack[top];
			stack[top] = stack[addr];
			break;
		case ASTO:
			addr = stack[top - 1];
			stack[addr] = stack[top];
			stack[top - 1] = stack[top];
			top --;
			// printf("%d\n", stack[top]);
			// reserve value for assign expr
			// top--;
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
			overflow(top);
			break;
		case JMP:
			pc = i.a + pc - 1;
			break;
		case JPC:
			if (stack[top] == 0)
				pc = i.a + pc - 1;
			top--;
			break;
		case JZ:
			if(stack[top] == 0)
				pc = i.a + pc - 1;
			top --;
			break;
		case JNZ:
			if(stack[top] != 0)
				pc = i.a + pc - 1;
			top --;
			break;
		case JOD:
			if(stack[top] % 2 == 1)
				pc = i.a + pc - 1;
			top --;
			break;
		case JE:
			if(stack[top - 1] == stack[top])
				pc = i.a + pc - 1;
			top -= 2;
			break;
		case JNE:
			if(stack[top - 1] != stack[top])
				pc = i.a + pc - 1;
			top -= 2;
			break;
		case JG:
			if(stack[top - 1] > stack[top])
				pc = i.a + pc - 1;
			top -= 2;
			break;
		case JGE:
			if(stack[top - 1] >= stack[top])
				pc = i.a + pc - 1;
			top -= 2;
			break;
		case JL:
			if(stack[top - 1] < stack[top])
				pc = i.a + pc - 1;
			top -= 2;
			break;
		case JLE:
			if(stack[top - 1] <= stack[top])
				pc = i.a + pc - 1;
			top -= 2;
			break;
		case OUT:
			for(j = top - i.a + 1; j <= top; j ++)
				printf("%d\t", stack[j]);
			printf("\n");
			top -= i.a;
			break;
		case RET:
			top = b - 1;
			pc = stack[b + 2];
			b = stack[b + 1];
			break;
		case RAND:
			srand(time(&temp));
			stack[top] = rand()%stack[top];
			printf("random: %d\n", stack[top]);
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
		statbegsys = createset(SYM_INC, SYM_DEC, SYM_IDENTIFIER, SYM_PRINT, SYM_RAND, SYM_RETURN, SYM_EXIT, SYM_CONTINUE, SYM_BREAK, SYM_IF, SYM_WHILE, SYM_DO, SYM_FOR, SYM_BEGIN, SYM_NULL);
		facbegsys = createset(SYM_IDENTIFIER, SYM_NUMBER, SYM_INC, SYM_DEC, SYM_LPAREN, SYM_MINUS, SYM_LOGIC_NOT, SYM_BIT_NOT, SYM_RAND, SYM_NULL);

		err = cc = cx = ll = 0; // initialize global variables
		ch = ' ';
		kk = MAXIDLEN;
		level = 0;
		tx = 0;
		ax = 0;
		px = 0;
		//id_read_ahead = 0;
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
