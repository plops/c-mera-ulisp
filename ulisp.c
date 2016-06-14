#include <setjmp.h>
#include <stdio.h>
#include <stdint.h>
typedef uintptr_t uintgr;
typedef intptr_t intgr;
typedef struct cons_object cons_object;

struct cons_object
{
	cons_object *car;
	cons_object *cdr;
};
typedef struct cons_symbol cons_symbol;

struct cons_symbol
{
	uintgr type;
	uintgr name;
};
typedef struct cons_number cons_number;

struct cons_number
{
	uintgr type;
	intgr integer;
};
cons_object *freelist;
cons_object *tee;
cons_object *global_env;
cons_object *gc_stack;
uintgr freespace;
cons_object workspace[315];
jmp_buf exception;
char buffer[17 + 1];

void init_workspace(void)
{
	freelist = 0;
	for(intgr i = 315 - 1; 0 <= i; --i){
		struct cons_object *obj = workspace + i;
		((cons_object*)obj)->car = 0;
		((cons_object*)obj)->cdr = freelist;
		freelist = obj;
		freespace++;
	}
}

void erro(const char *string)
{
	printf("Error: %s\n", string);
	gc_stack = 0;
	longjmp(exception, 1);
}

cons_object *myalloc(void)
{
	if (0 == freespace) {
		erro("No room");
	}
	cons_object *temp = freelist;
	freelist = ((cons_object*)freelist)->cdr;
	freespace--;
	return temp;
}

void myfree(cons_object *obj)
{
	((cons_object*)obj)->cdr = freelist;
	freelist = obj;
	freespace++;
}

cons_object *make_number(intgr n)
{
	cons_number *ptr = ((cons_number*)myalloc());
	ptr->type = 2;
	ptr->integer = n;
	return ((cons_object*)ptr);
}

cons_object *make_cons(cons_object *arg1, cons_object *arg2)
{
	cons_object *ptr = ((cons_object*)myalloc());
	ptr->car = arg1;
	ptr->cdr = arg2;
	return ptr;
}

cons_object *make_csymbol(uintgr name)
{
	cons_symbol *ptr = ((cons_symbol*)myalloc());
	ptr->type = 1;
	ptr->name = name;
	return ((cons_object*)ptr);
}

void mark_object(cons_object *obj)
{
	if (0 == obj) {
		return;
	}
	if (0 != (((uintgr)((cons_object*)obj)->car) & (__UINT64_C(1) << ((8 * sizeof(uintptr_t)) - 1)))) {
		return;
	}
	cons_object *arg = ((cons_object*)obj)->car;
	intgr type = ((cons_number*)obj)->type;
	((cons_object*)obj)->car = ((cons_object*)(((uintgr)((cons_object*)obj)->car) | (__UINT64_C(1) << ((8 * sizeof(uintptr_t)) - 1))));
	if ((1 != type) && (2 != type)) {
		mark_object(arg);
		mark_object(((cons_object*)obj)->cdr);
	}
}

void sweep(void)
{
	freelist = 0;
	freespace = 0;
	for(int i = 315 - 1; 0 <= i; --i){
		cons_object *obj = workspace + i;
		if (1 == (0 != (((uintgr)((cons_object*)obj)->car) & (__UINT64_C(1) << ((8 * sizeof(uintptr_t)) - 1))))) {
			((cons_object*)obj)->car = ((cons_object*)(((uintgr)((cons_object*)obj)->car) & ((__UINT64_C(1) << ((8 * sizeof(uintptr_t)) - 1)) - 1)));
		}
		else {
			((cons_object*)obj)->car = ((cons_object*)0);
			((cons_object*)obj)->cdr = freelist;
			freelist = obj;
			freespace++;
		}
	}
}

void gc(cons_object *form, cons_object *env)
{
	mark_object(tee);
	mark_object(global_env);
	mark_object(gc_stack);
	mark_object(form);
	mark_object(env);
	sweep();
}

intgr toradix40(intgr ch)
{
	if (0 == ch) {
		return 0;
	}
	if (('0' <= ch) && (ch <= '9')) {
		return 30 + (ch - '0');
	}
	ch = ch | 32;
	if (('a' <= ch) && (ch <= 'z')) {
		return 1 + (ch - 'a');
	}
	erro("ill. char in sym");
	return 0;
}

intgr fromradix40(intgr n)
{
	if ((1 <= n) && (n <= 26)) {
		return n + 'a' + -1;
	}
	if ((1 <= 30) && (n <= 39)) {
		return n + '0' + -30;
	}
	if (27 == n) {
		return '-';
	}
	return 0;
}

intgr pack40(char *c)
{
	return (40 * ((40 * toradix40(c[0])) + toradix40(c[1]))) + toradix40(c[2]);
}

intgr digitvalue(char d)
{
	if (('0' <= d) && (d <= '9')) {
		return d - '0';
	}
	d = d | 32;
	if (('a' <= d) && (d <= 'f')) {
		return 10 + (d - 'a');
	}
	return 16;
}

char *name(cons_object *obj)
{
	buffer[3] = ((char)0);
	if (1 != ((cons_symbol*)obj)->type) {
		erro("name");
	}
}

int main(void)
{
	printf("%lx\n", __UINT64_C(1) << ((8 * sizeof(uintptr_t)) - 1));
	return 0;
}