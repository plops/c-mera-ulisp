#include <setjmp.h>
#include <stdio.h>
#include <stdint.h>
//I use integers that have the same size as a pointer
typedef uintptr_t uintgr;
typedef intptr_t intgr;
//C-mera doesn't support unions
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
const char builtin_name[29][9] = { "symbols", "nil", "tee", "lambda", "let", "letstar", "closure", "specfrms", "quote", "defun", "defvar", "setq", "loop", "push", "pop", "incf", "decf", "progn", "return", "if", "cond", "and", "or", "functions", "not", "cons", "atom", "listp", "apply" };
cons_object *freelist;
cons_object *tee;
cons_object *global_env;
cons_object *gc_stack;
uintgr freespace;
cons_object workspace[315];
jmp_buf exception;
char buffer[9 + 1];

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

cons_object *_alloc(void)
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

cons_object *_number(intgr n)
{
	cons_number *ptr = ((cons_number*)_alloc());
	ptr->type = 2;
	ptr->integer = n;
	return ((cons_object*)ptr);
}

cons_object *_cons(cons_object *arg1, cons_object *arg2)
{
	cons_object *ptr = ((cons_object*)_alloc());
	ptr->car = arg1;
	ptr->cdr = arg2;
	return ptr;
}

cons_object *_symbol(uintgr name)
{
	cons_symbol *ptr = ((cons_symbol*)_alloc());
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

uintgr pack40(char *c)
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

char *lookupstring(uintgr name)
{
	for(int i = 0; i < 9; ++i){
		buffer[i] = builtin_name[name][i];
	}
	return buffer;
}

char *name(cons_object *obj)
{
	buffer[3] = ((char)0);
	if (1 != ((cons_symbol*)obj)->type) {
		erro("name");
	}
	uintgr x = ((cons_symbol*)obj)->name;
	if (x < 29) {
		return lookupstring(x);
	}
	for(int n = 2; 0 <= n; --n){
		buffer[n] = fromradix40(x % 40);
		x = x / 40;
	}
	return buffer;
}

intgr _integer(cons_object *obj)
{
	if (2 != ((cons_symbol*)obj)->type) {
		erro("not number");
	}
	return ((cons_number*)obj)->integer;
}

int issymbol(cons_object *obj, uintgr n)
{
	return (1 == ((cons_symbol*)obj)->type) && ((cons_symbol*)n)->name;
}

int _eq(cons_object *a, cons_object *b)
{
	return (a == b) || ((1 == ((cons_symbol*)a)->type) && (1 == ((cons_symbol*)b)->type) && (((cons_symbol*)a)->name == ((cons_symbol*)b)->name)) || ((2 == ((cons_symbol*)a)->type) && (2 == ((cons_symbol*)b)->type) && (((cons_number*)a)->integer == ((cons_number*)b)->integer));
}

cons_object *value(uintgr n, cons_object *env)
{
	while (NULL != env) {
		cons_object *item = ((cons_object*)env)->car;
		if (n == ((cons_symbol*)((cons_object*)item)->car)->name) {
			return item;
		}
		env = ((cons_object*)env)->cdr;
	}
	return NULL;
}

cons_object *findvalue(cons_object *var, cons_object *env)
{
	uintgr varname = ((cons_symbol*)var)->name;
	cons_object *pair = value(varname, env);
	if (NULL == pair) {
		pair = value(varname, global_env);
	}
	if (NULL == pair) {
		erro("unknown var");
	}
	return pair;
}

cons_object *findtwin(cons_object *var, cons_object *env)
{
	while (NULL != env) {
		cons_object *item = ((cons_object*)env)->car;
		if (var == ((cons_object*)item)->car) {
			return item;
		}
		env = ((cons_object*)env)->cdr;
	}
	return NULL;
}

cons_object *closure(int tail, cons_object *fname, cons_object *state, cons_object *function, cons_object *args, cons_object **env)
{
	cons_object *params = ((cons_object*)function)->car;
	function = ((cons_object*)function)->cdr;
	//push state if not already in env
	while (NULL != state) {
		cons_object *item = ((cons_object*)state)->car;
		if (NULL == findtwin(((cons_object*)item)->car, *env)) {
			*env = _cons(item, *env);
		}
		state = ((cons_object*)state)->cdr;
	}
	//add arguments to environment
	while ((NULL != params) && (NULL != args)) {
		object_cons *var = ((cons_object*)params)->car;
		object_cons *value = ((cons_object*)args)->car;
		if (tail) {
			{
				cons_object *item = findtwin(var, *env);
				if (NULL != item) {
					((cons_object*)item)->cdr = value;
				}
				else {
					*env = _cons(_cons(var, value), *env);
				}
			}
		}
		else {
			*env = _cons(_cons(var, value), *env);
		}
		params = ((cons_object*)params)->cdr;
		args = ((cons_object*)args)->cdr;
	}
	if (NULL != params) {
		erro("too few params");
	}
	if (NULL != args) {
		erro("too many params");
	}
	//do implicit progn
	return tf_progn(function, *env);
}

int listlength(cons_object *list)
{
	int len = 0;
	while (NULL != list) {
		list = ((cons_object*)list)->cdr;
		len++;
	}
	return len;
}
typedef object *(*fn_ptr_type)(object *, object *);

cons_object *_apply(cons_object *function, cons_object *args, cons_object **env)
{
	if (1 == ((cons_symbol*)function)->type) {
		uintgr name = ((cons_symbol*)function)->name;
		int nargs = listlength(args);
		if (29 <= name) {
			erro("not a function");
		}
		if (nargs < lookupmin(name)) {
			erro("too few args");
		}
		if (lookupmin(name) < nargs) {
			erro("too many args");
		}
		return ((fn_ptr_type)lookupfn(name))(args, *env);
	}
	if (listp(function) && issymbol(((cons_object*)function)->car, 3)) {
		function = ((cons_object*)function)->cdr;
		cons_object *result = closure(0, NULL, NULL, function, args, env);
		return _eval(result, *env);
	}
	if (listp(function) && issymbol(((cons_object*)function)->car, 6)) {
		function = ((cons_object*)function)->cdr;
		{
			cons_object *result = closure(0, NULL, ((cons_object*)function)->car, ((cons_object*)function)->cdr, args, env);
			return _eval(result, *env);
		}
	}
	erro("illegal function");
	return NULL;
}
//checked car and cdr

cons_object *carx(cons_object *arg)
{
	if (0 == listp(arg)) {
		erro("can't take car");
	}
	if (NULL == arg) {
		return NULL;
	}
	return ((cons_object*)arg)->car;
}

cons_object *cdrx(cons_object *arg)
{
	if (0 == listp(arg)) {
		erro("can't take cdr");
	}
	if (NULL == arg) {
		return NULL;
	}
	return ((cons_object*)arg)->cdr;
}

cons_object *SP_quote(cons_object *args, cons_object *env)
{
	(void) env;
	return ((cons_object*)args)->car;
}

cons_object *SP_defun(cons_object *args, cons_object *env)
{
	(void) env;
	cons_object *var = ((cons_object*)args)->car;
	if (1 != ((cons_symbol*)var)->type) {
		erro("not a symbol");
	}
	cons_object *val = _cons(_symbol(3), ((cons_object*)args)->cdr);
	cons_object *pair = value(((cons_symbol*)var)->name, global_env);
	if (NULL != pair) {
		((cons_object*)pair)->cdr = val;
		return var;
	}
	global_env = _cons(_cons(var, val), global_env);
	return var;
}

cons_object *SP_defvar(cons_object *args, cons_object *env)
{
	cons_object *var = ((cons_object*)args)->car;
	if (1 != ((cons_symbol*)var)->type) {
		erro("not a symbol");
	}
	cons_object *val = _eval(((cons_object*)((cons_object*)args)->cdr)->car, env);
	cons_object *pair = value(((cons_symbol*)var)->name, global_env);
	if (NULL != pair) {
		((cons_object*)pair)->cdr = val;
	}
	else {
		return var;
	}
	global_env = _cons(_cons(var, val), global_env);
	return var;
}

cons_object *SP_setq(cons_object *args, cons_object *env)
{
	cons_object *arg = _eval(((cons_object*)((cons_object*)args)->cdr)->car, env);
	cons_object *pair = findvalue(((cons_object*)args)->car, env);
	((cons_object*)pair)->cdr = arg;
	return arg;
}

cons_object *_eval(cons_object *form, cons_object *env)
{
	int TC = 0;
	EVAL:
	if (freespace < 10) {
		gc(form, env);
	}
	if (NULL == form) {
		return NULL;
	}
	if (2 == ((cons_symbol*)form)->type) {
		return form;
	}
	if (1 == ((cons_symbol*)form)->type) {
		cons_object *name = ((cons_symbol*)form)->name;
		if (1 == name) {
			return;
		}
		cons_object *pair = value(name, env);
		if (NULL != pair) {
			return ((cons_object*)pair)->cdr;
		}
		pair = value(name, global_env);
		if (NULL != pair) {
			return ((cons_object*)pair)->cdr;
		}
		else if (name <= 29) 
		{
			return form;
		}
		erro("undefined");
	}
	//it's a list
	cons_object *function = ((cons_object*)form)->car;
	cons_object *args = ((cons_object*)form)->cdr;
	//list starting with symbol?
	if (1 == ((cons_symbol*)function)->type) {
		{
			uintgr name = ((cons_symbol*)function)->name;
			if (4 == name) {
				cons_object *assigns = ((cons_object*)args)->car;
				cons_object *forms = ((cons_object*)args)->cdr;
				cons_object *newenv = env;
				while (NULL != assigns) {
					cons_object *assign = ((cons_object*)assigns)->car;
					if ((2 != ((cons_symbol*)assign)->type) && (1 != ((cons_symbol*)assign)->type) && (NULL != assign)) {
						newenv = _cons(_cons(((cons_object*)assign)->car, _eval(((cons_object*)((cons_object*)assign)->cdr)->car, env)), newenv);
					}
					else {
						newenv = _cons(_cons(assign, NULL), newenv);
					}
					assigns = ((cons_object*)assigns)->cdr;
				}
			}
		}
	}
}

int main(void)
{
	return 0;
}