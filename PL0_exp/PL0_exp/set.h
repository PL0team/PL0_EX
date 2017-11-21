#ifndef SET_H
#define SET_H

typedef struct snode
{
	int elem;
	struct snode* next;
} snode, *symset;	//order link list ex:	<4, 3, 2, 1>

symset phi, declbegsys, statbegsys, facbegsys, relset;

symset createset(int data, .../* SYM_NULL */);
void destroyset(symset s);
symset uniteset(symset s1, symset s2);
int inset(int elem, symset s);

typedef symset codelist;

codelist createlist();
void destroylist(codelist l);
void insertlist(codelist l, int elem);
void unitelist(codelist dst, codelist src);
int isempty(codelist l);

#endif
// EOF set.h
