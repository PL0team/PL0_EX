

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include "set.h"

symset uniteset(symset s1, symset s2)
{
	symset s;
	snode* p;

	s1 = s1->next;
	s2 = s2->next;

	s = p = (snode*) malloc(sizeof(snode));
	while (s1 && s2)  // is not null
	{
		p->next = (snode*) malloc(sizeof(snode));
		p = p->next;
		if (s1->elem < s2->elem)
		{
			p->elem = s1->elem;
			s1 = s1->next;
		}
		else
		{
			p->elem = s2->elem;
			s2 = s2->next;
		}
	}

	while (s1)
	{
		p->next = (snode*) malloc(sizeof(snode));
		p = p->next;
		p->elem = s1->elem;
		s1 = s1->next;

	}

	while (s2)
	{
		p->next = (snode*) malloc(sizeof(snode));
		p = p->next;
		p->elem = s2->elem;
		s2 = s2->next;
	}

	p->next = NULL;

	return s;
} // uniteset

void setinsert(symset s, int elem)
{
	snode* p = s;
	snode* q;

	while (p->next && p->next->elem < elem)
	{
		p = p->next;
	}

	q = (snode*) malloc(sizeof(snode));
	q->elem = elem;
	q->next = p->next;
	p->next = q;
} // setinsert

symset createset(int elem, .../* SYM_NULL */)
{
	va_list list;
	symset s;

	s = (snode*) malloc(sizeof(snode));
	s->next = NULL;

	va_start(list, elem);
	while (elem)
	{
		setinsert(s, elem);
		elem = va_arg(list, int);
	}
	va_end(list);
	return s;
} // createset

void destroyset(symset s)
{
	snode* p;

	while (s)
	{
		p = s;
		p->elem = -1000000;
		s = s->next;
		free(p);
	}
} // destroyset

int inset(int elem, symset s)
{
	s = s->next;
	while (s && s->elem < elem)
		s = s->next;

	if (s && s->elem == elem)
		return 1;
	else
		return 0;
} // inset

codelist createlist()
{
	codelist l;
	l = (snode*)malloc(sizeof(snode));
	l->next = NULL;
	return l;
} // createlist

void destroylist(codelist l)
{
	destroyset(l);
} // destroylist

void insertlist(codelist l, int elem)
{
	setinsert(l, elem);
} // insertlist

void unitelist(codelist dst, codelist src)
{
	snode *s1, *s2;
	s1 = dst;
	s2 = src->next;
	while(s2 != NULL)
	{
		src->next = s2->next;
		while(s1->next != NULL && s1->next->elem < s2->elem)
			s1 = s1->next;
		if(s1->next != NULL)
		{
			if(s1->next->elem > s2->elem)
			{
				s2->next = s1->next;
				s1->next = s2;
			}
			else
			{
				s2->elem = -1000000;
				free(s2);
			}
		}
		else
		{
			s2->next = s1->next;
			s1->next = s2;
		}
		s2 = src->next;
	}
} // unitelist

int isempty(codelist l)
{
	if(l->next == NULL)
		return 1;
	else
		return 0;
} // isempty

// EOF set.c
