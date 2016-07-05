//Headers
#include <x86intrin.h>
//__rdtsc intrinsic
#include <stdint.h>
//uintptr_t
#include <stdlib.h>
//exit
#include <unistd.h>
//write
#include <string.h>
//strcmp
//Type declarations
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
typedef cons_object *(*fn_ptr_type)(cons_object *, cons_object *);
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
typedef cons_object *o;
#undef NULL
o freelist;
o tee;
o global_env;
o gc_stack;
uintgr freespace;
cons_object workspace[16 * 1024];
char return_flag = 0;
char buffer[7 + 1];
char last_char;
void *NULL = 0;
//forward declarations

o sp_decf(o args, o env)
;

o sp_incf(o args, o env)
;

o sp_pop(o args, o env)
;

o sp_push(o args, o env)
;

o sp_loop(o args, o env)
;

o sp_setq(o args, o env)
;

o sp_defvar(o args, o env)
;

o sp_defun(o args, o env)
;

o sp_quote(o args, o env)
;

o tf_or(o args, o env)
;

o tf_and(o args, o env)
;

o tf_cond(o args, o env)
;

o tf_if(o args, o env)
;

o tf_return(o args, o env)
;

o tf_progn(o args, o env)
;

o fn_third(o args, o env)
;

o fn_second(o args, o env)
;

o fn_reverse(o args, o env)
;

o fn_length(o args, o env)
;

o fn_list(o args, o env)
;

o fn_assoc(o args, o env)
;

o fn_princ(o args, o env)
;

o fn_less(o args, o env)
;

o fn_add(o args, o env)
;

o fn_apply(o args, o env)
;

o fn_cdr(o args, o env)
;

o fn_car(o args, o env)
;

o fn_eq(o args, o env)
;

o fn_listp(o args, o env)
;

o fn_atom(o args, o env)
;

o fn_cons(o args, o env)
;

o fn_not(o args, o env)
;

void putui(uintgr i)
;

void puti(intgr i)
;

int _getchar(void)
;

void _putchar(int c)
;

void _putsn(char *string, int len)
;

void repl(o env)
;

o _read(void)
;

void _print_object(o form)
;

int _strlen(const char *s)
;

o read_rest(void)
;

o nextitem(void)
;

int _isspace(int c)
;

int _getc(void)
;

void init_env(void)
;

o _eval(o form, o env)
;

o cdrx(o arg)
;

o carx(o arg)
;

o _apply(o function, o args, o *env)
;

fn_ptr_type lookupfn(uintgr idx)
;

int lookupmax(uintgr idx)
;

int lookupmin(uintgr idx)
;

int builtin(char *name)
;

int _string_eq_p(const char *a, const char *b, int n)
;

int listlength(o list)
;

o closure(int tail, o fname, o state, o function, o args, o *env)
;

o findtwin(o var, o env)
;

o findvalue(o var, o env)
;

o value(uintgr n, o env)
;

int _eq(o a, o b)
;

int issymbol(o obj, uintgr n)
;

intgr _integer(o obj)
;

char *name(o obj)
;

char *lookupstring(uintgr idx)
;

intgr digitvalue(char d)
;

uintgr pack40(char *c)
;

intgr fromradix40(intgr n)
;

intgr toradix40(intgr ch)
;

void gc(o form, o env)
;

void sweep(void)
;

void mark_object(o obj)
;

o _symbol(uintgr name)
;

o _cons(o arg1, o arg2)
;

o _number(intgr n)
;

o _alloc(void)
;

void init_workspace(void)
;
//Global variables
const char builtin_name[37][7] = { "nil", "tee", "lambda", "let", "closure", "decf", "incf", "pop", "push", "loop", "setq", "defvar", "defun", "quote", "or", "and", "cond", "if", "return", "progn", "third", "second", "reverse", "length", "list", "assoc", "princ", "less", "add", "apply", "cdr", "car", "eq", "listp", "atom", "cons", "not" };
const char builtin_par_max[37] = { 0, 0, 0, 0, 0, 2, 2, 1, 2, 127, 2, 127, 127, 1, 127, 127, 127, 3, 127, 127, 1, 1, 1, 1, 127, 2, 1, 127, 127, 127, 1, 1, 2, 1, 1, 2, 1 };
const char builtin_par_min[37] = { 0, 0, 0, 0, 0, 1, 1, 1, 2, 0, 2, 0, 0, 1, 0, 0, 0, 2, 0, 0, 1, 1, 1, 1, 0, 2, 1, 1, 0, 2, 1, 1, 2, 1, 1, 2, 1 };
fn_ptr_type builtin_fptr[37] = { 0, 0, 0, 0, 0, sp_decf, sp_incf, sp_pop, sp_push, sp_loop, sp_setq, sp_defvar, sp_defun, sp_quote, tf_or, tf_and, tf_cond, tf_if, tf_return, tf_progn, fn_third, fn_second, fn_reverse, fn_length, fn_list, fn_assoc, fn_princ, fn_less, fn_add, fn_apply, fn_cdr, fn_car, fn_eq, fn_listp, fn_atom, fn_cons, fn_not };

o sp_decf(o args, o env)
{
	//minimum number of parameters: 1, max. nr. of parameters: 2
	o var = ((o)args)->car;
	o pair = findvalue(var, env);
	int result = _integer(_eval(var, env));
	int temp = 1;
	if (NULL != ((o)args)->cdr) {
		temp = _integer(_eval(((o)((o)args)->cdr)->car, env));
	}
	result = result - temp;
	var = _number(result);
	((o)pair)->cdr = var;
	return var;
}

o sp_incf(o args, o env)
{
	//minimum number of parameters: 1, max. nr. of parameters: 2
	o var = ((o)args)->car;
	o pair = findvalue(var, env);
	int result = _integer(_eval(var, env));
	int temp = 1;
	if (NULL != ((o)args)->cdr) {
		temp = _integer(_eval(((o)((o)args)->cdr)->car, env));
	}
	result = result + temp;
	var = _number(result);
	((o)pair)->cdr = var;
	return var;
}

o sp_pop(o args, o env)
{
	//minimum number of parameters: 1, max. nr. of parameters: 1
	o pair = findvalue(((o)((o)args)->cdr)->car, env);
	o result = ((o)((o)pair)->cdr)->car;
	((o)pair)->cdr = ((o)((o)pair)->cdr)->cdr;
	return result;
}

o sp_push(o args, o env)
{
	//minimum number of parameters: 2, max. nr. of parameters: 2
	o item = _eval(((o)args)->car, env);
	o pair = findvalue(((o)((o)args)->cdr)->car, env);
	((o)pair)->cdr = _cons(item, ((o)pair)->cdr);
	return ((o)pair)->cdr;
}

o sp_loop(o args, o env)
{
	//minimum number of parameters: 0, max. nr. of parameters: 127
	return_flag = 0;
	o start = args;
	for(; ; ){
		args = start;
		while (NULL != args) {
			o form = ((o)args)->car;
			((void)form);
			o result = _eval(form, env);
			if (1 == return_flag) {
				return_flag = 0;
				return result;
			}
			args = ((o)args)->cdr;
		}
	}
}

o sp_setq(o args, o env)
{
	//minimum number of parameters: 2, max. nr. of parameters: 2
	o arg = _eval(((o)((o)args)->cdr)->car, env);
	o pair = findvalue(((o)args)->car, env);
	((o)pair)->cdr = arg;
	return arg;
}

o sp_defvar(o args, o env)
{
	//minimum number of parameters: 0, max. nr. of parameters: 127
	o var = ((o)args)->car;
	if (1 != ((cons_symbol*)var)->type) {
		_putsn("(not a symbol)", 14);
		_putsn("EXIT\n", 6);
		exit(0);
	}
	o val = _eval(((o)((o)args)->cdr)->car, env);
	o pair = value(((cons_symbol*)var)->name, global_env);
	if (NULL != pair) {
		((o)pair)->cdr = val;
		return var;
	}
	global_env = _cons(_cons(var, val), global_env);
	return var;
}

o sp_defun(o args, o env)
{
	//minimum number of parameters: 0, max. nr. of parameters: 127
	(void) env;
	o var = ((o)args)->car;
	if (1 != ((cons_symbol*)var)->type) {
		_putsn("(not a symbol)", 14);
		_putsn("EXIT\n", 6);
		exit(0);
	}
	o val = _cons(_symbol(2), ((o)args)->cdr);
	o pair = value(((cons_symbol*)var)->name, global_env);
	if (NULL != pair) {
		((o)pair)->cdr = val;
		return var;
	}
	global_env = _cons(_cons(var, val), global_env);
	return var;
}

o sp_quote(o args, o env)
{
	//minimum number of parameters: 1, max. nr. of parameters: 1
	(void) env;
	return ((o)args)->car;
}

o tf_or(o args, o env)
{
	//minimum number of parameters: 0, max. nr. of parameters: 127
	o more = ((o)args)->cdr;
	while (NULL != more) {
		o e = ((o)more)->car;
		((void)e);
		o result = _eval(((o)args)->car, env);
		if (NULL != result) {
			return result;
		}
		args = more;
		more = ((o)more)->cdr;
	}
	return ((o)args)->car;
}

o tf_and(o args, o env)
{
	//minimum number of parameters: 0, max. nr. of parameters: 127
	if (NULL == args) {
		return tee;
	}
	o more = ((o)args)->cdr;
	while (NULL != more) {
		o e = ((o)more)->car;
		((void)e);
		if (NULL == _eval(((o)args)->car, env)) {
			return NULL;
		}
		args = more;
		more = ((o)more)->cdr;
	}
	return ((o)args)->car;
}

o tf_cond(o args, o env)
{
	//minimum number of parameters: 0, max. nr. of parameters: 127
	while (NULL != args) {
		o clause = ((o)args)->car;
		((void)clause);
		o test = _eval(((o)clause)->car, env);
		o forms = ((o)clause)->cdr;
		if (NULL != test) {
			if (NULL == forms) {
				return test;
			}
			else {
				return tf_progn(forms, env);
			}
		}
		args = ((o)args)->cdr;
	}
	return NULL;
}

o tf_if(o args, o env)
{
	//minimum number of parameters: 2, max. nr. of parameters: 3
	if (NULL != _eval(((o)args)->car, env)) {
		return ((o)((o)args)->cdr)->car;
	}
	return ((o)((o)((o)args)->cdr)->cdr)->car;
}

o tf_return(o args, o env)
{
	//minimum number of parameters: 0, max. nr. of parameters: 127
	return_flag = 1;
	return tf_progn(args, env);
}

o tf_progn(o args, o env)
{
	//minimum number of parameters: 0, max. nr. of parameters: 127
	if (NULL == args) {
		return NULL;
	}
	o more = ((o)args)->cdr;
	while (NULL != more) {
		o e = ((o)more)->car;
		((void)e);
		_eval(((o)args)->car, env);
		args = more;
		more = ((o)more)->cdr;
	}
	return ((o)args)->car;
}

o fn_third(o args, o env)
{
	//minimum number of parameters: 1, max. nr. of parameters: 1
	(void) env;
	return carx(cdrx(cdrx(((o)args)->car)));
}

o fn_second(o args, o env)
{
	//minimum number of parameters: 1, max. nr. of parameters: 1
	(void) env;
	return carx(cdrx(((o)args)->car));
}

o fn_reverse(o args, o env)
{
	//minimum number of parameters: 1, max. nr. of parameters: 1
	(void) env;
	(void) env;
	o list = ((o)args)->car;
	if (0 == ((2 != ((cons_symbol*)list)->type) && (1 != ((cons_symbol*)list)->type))) {
		_putsn("(arg not list)", 14);
		_putsn("EXIT\n", 6);
		exit(0);
	}
	o result = NULL;
	while (NULL != list) {
		o e = ((o)list)->car;
		((void)e);
		result = _cons(e, result);
		list = ((o)list)->cdr;
	}
	return result;
}

o fn_length(o args, o env)
{
	//minimum number of parameters: 1, max. nr. of parameters: 1
	(void) env;
	(void) env;
	o list = ((o)args)->car;
	if (0 == ((2 != ((cons_symbol*)list)->type) && (1 != ((cons_symbol*)list)->type))) {
		_putsn("(arg not list)", 14);
		_putsn("EXIT\n", 6);
		exit(0);
	}
	return _number(listlength(list));
}

o fn_list(o args, o env)
{
	//minimum number of parameters: 0, max. nr. of parameters: 127
	(void) env;
	(void) env;
	return args;
}

o fn_assoc(o args, o env)
{
	//minimum number of parameters: 2, max. nr. of parameters: 2
	(void) env;
	(void) env;
	o key = ((o)args)->car;
	o list = ((o)((o)args)->cdr)->car;
	if (0 == ((2 != ((cons_symbol*)list)->type) && (1 != ((cons_symbol*)list)->type))) {
		_putsn("(last arg not list)", 19);
		_putsn("EXIT\n", 6);
		exit(0);
	}
	while (NULL != list) {
		o pair = ((o)list)->car;
		((void)pair);
		if (1 == _eq(key, ((o)pair)->car)) {
			return pair;
		}
		list = ((o)list)->cdr;
	}
	return NULL;
}

o fn_princ(o args, o env)
{
	//minimum number of parameters: 1, max. nr. of parameters: 1
	(void) env;
	(void) env;
	o obj = ((o)args)->car;
	_print_object(obj);
	return obj;
}

o fn_less(o args, o env)
{
	//minimum number of parameters: 1, max. nr. of parameters: 127
	(void) env;
	(void) env;
	intgr arg1 = _integer(((o)args)->car);
	args = ((o)args)->cdr;
	while (NULL != args) {
		o item = ((o)args)->car;
		((void)item);
		intgr arg2 = _integer(item);
		if (0 == (arg1 < arg2)) {
			return NULL;
		}
		arg1 = arg2;
		args = ((o)args)->cdr;
	}
	return tee;
}

o fn_add(o args, o env)
{
	//minimum number of parameters: 0, max. nr. of parameters: 127
	(void) env;
	(void) env;
	intgr result = 0;
	while (NULL != args) {
		o item = ((o)args)->car;
		((void)item);
		intgr temp = _integer(item);
		result = result + temp;
		args = ((o)args)->cdr;
	}
	return _number(result);
}

o fn_apply(o args, o env)
{
	//minimum number of parameters: 2, max. nr. of parameters: 127
	(void) env;
	o previous = NULL;
	o last = args;
	o G1271 = ((o)last)->cdr;
	while (NULL != G1271) {
		o e = ((o)G1271)->car;
		((void)e);
		previous = last;
		G1271 = ((o)G1271)->cdr;
	}
	if (0 == ((2 != ((cons_symbol*)((o)last)->car)->type) && (1 != ((cons_symbol*)((o)last)->car)->type))) {
		_putsn("(last arg not list)", 19);
		_putsn("EXIT\n", 6);
		exit(0);
	}
	((o)previous)->cdr = ((o)last)->car;
	return _apply(((o)args)->car, ((o)args)->cdr, &env);
}

o fn_cdr(o args, o env)
{
	//minimum number of parameters: 1, max. nr. of parameters: 1
	(void) env;
	(void) env;
	return cdrx(((o)args)->car);
}

o fn_car(o args, o env)
{
	//minimum number of parameters: 1, max. nr. of parameters: 1
	(void) env;
	(void) env;
	return carx(((o)args)->car);
}

o fn_eq(o args, o env)
{
	//minimum number of parameters: 2, max. nr. of parameters: 2
	(void) env;
	(void) env;
	o arg1 = ((o)args)->car;
	o arg2 = ((o)((o)args)->cdr)->car;
	if (_eq(arg1, arg2)) {
		return tee;
	}
	else {
		return NULL;
	}
}

o fn_listp(o args, o env)
{
	//minimum number of parameters: 1, max. nr. of parameters: 1
	(void) env;
	(void) env;
	o arg1 = ((o)args)->car;
	if ((2 != ((cons_symbol*)arg1)->type) && (1 != ((cons_symbol*)arg1)->type)) {
		return tee;
	}
	else {
		return NULL;
	}
}

o fn_atom(o args, o env)
{
	//minimum number of parameters: 1, max. nr. of parameters: 1
	(void) env;
	(void) env;
	o arg1 = ((o)args)->car;
	if ((2 != ((cons_symbol*)arg1)->type) && (1 != ((cons_symbol*)arg1)->type) && (NULL != arg1)) {
		return NULL;
	}
	else {
		return tee;
	}
}

o fn_cons(o args, o env)
{
	//minimum number of parameters: 2, max. nr. of parameters: 2
	(void) env;
	(void) env;
	return _cons(((o)args)->car, ((o)((o)args)->cdr)->car);
}

o fn_not(o args, o env)
{
	//minimum number of parameters: 1, max. nr. of parameters: 1
	(void) env;
	(void) env;
	if (NULL == ((o)args)->car) {
		return tee;
	}
	else {
		return NULL;
	}
}

void putui(uintgr i)
{
	uintgr rev = 0;
	while (0 < i) {
		uintgr digit = i % 10;
		rev = (10 * rev) + digit;
		i = i / 10;
	}
	while (0 < rev) {
		{
			uintgr digit = rev % 10;
			_putchar(48 + digit);
			rev = rev / 10;
		}
	}
}

void puti(intgr i)
{
	if (i < 0) {
		_putchar(45);
		//output minus (-) character
		i = -i;
	}
	intgr rev = 0;
	while (0 < i) {
		int digit = i % 10;
		rev = (10 * rev) + digit;
		i = i / 10;
	}
	while (0 < rev) {
		{
			int digit = rev % 10;
			_putchar(48 + digit);
			rev = rev / 10;
		}
	}
}

int _getchar(void)
{
	char b;
	int num = read(0, &b, 1);
	if (num < 1) {
		return -1;
	}
	else {
		return ((int)b);
	}
}

void _putchar(int c)
{
	write(0, &c, 1);
}

void _putsn(char *string, int len)
{
	write(0, string, len);
}

void repl(o env)
{
	for(; ; ){
		gc(NULL, env);
		_putsn("freespace=", 10);
		puti(freespace);
		_putchar('\n');
		_putsn("> ", 2);
		o line = _read();
		if (NULL == line) {
			_putchar('\n');
			return;
		}
		_putchar('\n');
		gc_stack = _cons(line, gc_stack);
		_print_object(_eval(line, env));
		gc_stack = ((o)gc_stack)->cdr;
		_putchar('\n');
		_putchar('\n');
	}
}

o _read(void)
{
	o item = nextitem();
	if (((o)1) == item) {
		return read_rest();
	}
	if (((o)4) == item) {
		return _read();
	}
	if (((o)3) == item) {
		return _cons(_symbol(13), _cons(_read(), NULL));
	}
	return item;
}

void _print_object(o form)
{
	if (NULL == form) {
		_putsn("nil", 3);
	}
	else if (((2 != ((cons_symbol*)form)->type) && (1 != ((cons_symbol*)form)->type)) && issymbol(((o)form)->car, 4)) 
	{
		_putsn("<closure>", 9);
	}
	else if ((2 != ((cons_symbol*)form)->type) && (1 != ((cons_symbol*)form)->type)) 
	{
		_putchar(((int)'('));
		_print_object(((o)form)->car);
		form = ((o)form)->cdr;
		while ((NULL != form) && ((2 != ((cons_symbol*)form)->type) && (1 != ((cons_symbol*)form)->type))) {
			_putchar(' ');
			_print_object(((o)form)->car);
			form = ((o)form)->cdr;
		}
		if (NULL != form) {
			_putsn(" . ", 3);
			_print_object(form);
		}
		_putchar(')');
	}
	else if (2 == ((cons_symbol*)form)->type) 
	{
		puti(_integer(form));
	}
	else if (1 == ((cons_symbol*)form)->type) 
	{
		char *s = name(form);
		int len = _strlen(s);
		_putsn(s, len);
	}
	else {
		_putsn("(print err)", 11);
		_putsn("EXIT\n", 6);
		exit(0);
	}
}

int _strlen(const char *s)
{
	const char *start = s;
	while (*s) {
		s = 1 + s;
	}
	return s - start;
}

o read_rest(void)
{
	o item = nextitem();
	if (((o)2) == item) {
		return NULL;
	}
	if (((o)4) == item) {
		o arg1 = _read();
		if (NULL != read_rest()) {
			_putsn("(malformed list)", 16);
			_putsn("EXIT\n", 6);
			exit(0);
		}
		return arg1;
	}
	if (((o)3) == item) {
		{
			o arg1 = _read();
			return _cons(_cons(_symbol(13), _cons(arg1, NULL)), read_rest());
		}
	}
	if (((o)1) == item) {
		item = read_rest();
	}
	return _cons(item, read_rest());
}

o nextitem(void)
{
	int ch = _getc();
	while (_isspace(ch)) {
		ch = _getc();
	}
	if (';' == ch) {
		while ('(' != ch) {
			ch = _getc();
		}
		ch = '(';
	}
	if ('\n' == ch) {
		ch = _getc();
	}
	if (-1 == ch) {
		_putsn("EXIT", 4);
	}
	if (')' == ch) {
		return ((o)2);
	}
	if ('(' == ch) {
		return ((o)1);
	}
	if (39 == ch) {
		return ((o)3);
	}
	if ('.' == ch) {
		return ((o)4);
	}
	//parse var or number
	intgr index = 0;
	intgr base = 10;
	intgr sign = 1;
	uintgr result = 0;
	if ('+' == ch) {
		buffer[index++] = ch;
		ch = _getc();
	}
	else if ('-' == ch) 
	{
		sign = -1;
		buffer[index++] = ch;
		ch = _getc();
	}
	else if ('#' == ch) 
	{
		ch = _getc() | 32;
		if ('b' == ch) {
			base = 2;
		}
		else if ('o' == ch) 
		{
			base = 8;
		}
		else if ('x' == ch) 
		{
			base = 16;
		}
		else {
			_putsn("(illegal char after #)", 22);
			_putsn("EXIT\n", 6);
			exit(0);
		}
		ch = _getc();
	}
	intgr isnumber = digitvalue(ch) < base;
	//in case var is one letter
	buffer[2] = 0;
	while ((0 == _isspace(ch)) && (')' != ch) && (index < 7)) {
		buffer[index++] = ch;
		intgr temp = digitvalue(ch);
		result = temp + (result * base);
		isnumber = isnumber && (digitvalue(ch) < base);
		ch = _getc();
	}
	buffer[index] = 0;
	if (')' == ch) {
		last_char = ')';
	}
	if (isnumber) {
		if ((base == 10) && ((((uintgr)32767) + ((1 - sign) / 2)) < result)) {
			_putsn("(num out of range)", 18);
			_putsn("EXIT\n", 6);
			exit(0);
		}
		return _number(sign * result);
	}
	intgr x = builtin(buffer);
	if (x == 0) {
		return NULL;
	}
	if (x < 37) {
		return _symbol(x);
	}
	else {
		return _symbol(pack40(buffer));
	}
}

int _isspace(int c)
{
	if ((c == ' ') || (c == '\n') || (c == '\t') || (c == '\r') || (c == '') || (c == '\n')) {
		return 1;
	}
	else {
		return 0;
	}
}

int _getc(void)
{
	if (last_char) {
		int temp = last_char;
		last_char = 0;
		return temp;
	}
	{
		int temp = _getchar();
		_putchar(temp);
		return temp;
	}
}

void init_env(void)
{
	global_env = NULL;
	tee = _symbol(1);
}

o _eval(o form, o env)
{
	int TC = 0;
	EVALJUMP:
	if (freespace < 10) {
		gc(form, env);
	}
	if (NULL == form) {
		//NULL
		return NULL;
	}
	if (2 == ((cons_symbol*)form)->type) {
		//number
		return form;
	}
	if (1 == ((cons_symbol*)form)->type) {
		//symbol
		uintgr name = ((cons_symbol*)form)->name;
		if (0 == name) {
			//
			return NULL;
		}
		o pair = value(name, env);
		if (NULL != pair) {
			//sym cdr pair
			return ((o)pair)->cdr;
		}
		pair = value(name, global_env);
		if (NULL != pair) {
			//sym cdr pair in global env
			return ((o)pair)->cdr;
		}
		else if (name <= 37) 
		{
			//builtin declaration
			return form;
		}
		else {
			puti(name);
			_print_object(form);
			_putsn("(undefined variable)", 20);
			_putsn("EXIT\n", 6);
			exit(0);
		}
	}
	//it's a list
	o function = ((o)form)->car;
	o args = ((o)form)->cdr;
	if (1 == ((cons_symbol*)function)->type) {
		//list starting with symbol
		{
			uintgr name = ((cons_symbol*)function)->name;
			if (3 == name) {
				//let
				o assigns = ((o)args)->car;
				o forms = ((o)args)->cdr;
				o newenv = env;
				//process LET
				while (NULL != assigns) {
					o assign = ((o)assigns)->car;
					((void)assign);
					//go through assigns
					if ((2 != ((cons_symbol*)assign)->type) && (1 != ((cons_symbol*)assign)->type) && (NULL != assign)) {
						newenv = _cons(_cons(((o)assign)->car, _eval(((o)((o)assign)->cdr)->car, env)), newenv);
					}
					else {
						newenv = _cons(_cons(assign, NULL), newenv);
					}
					assigns = ((o)assigns)->cdr;
				}
				env = newenv;
				form = tf_progn(forms, env);
				TC = 1;
				goto EVALJUMP;
			}
			if (2 == name) {
				//process LAMBDA
				if (NULL == env) {
					//lambda nil env
					return form;
				}
				o envcopy = NULL;
				while (NULL != env) {
					{
						o pair = ((o)env)->car;
						((void)pair);
						o val = ((o)pair)->cdr;
						if (2 == ((cons_symbol*)val)->type) {
							//number
							val = _number(((cons_number*)val)->integer);
						}
						envcopy = _cons(_cons(((o)pair)->car, val), envcopy);
					}
					env = ((o)env)->cdr;
				}
				//call lambda
				return _cons(_symbol(4), _cons(envcopy, args));
			}
			if ((5 <= name) && (name <= 13)) {
				//process SPECIAL form
				return ((fn_ptr_type)lookupfn(name))(args, env);
			}
			if ((14 <= name) && (name <= 19)) {
				//process TAIL CALL form
				form = ((fn_ptr_type)lookupfn(name))(args, env);
				TC = 1;
				goto EVALJUMP;
			}
		}
	}
	//evaluate the parameters - result in head
	o fname = ((o)form)->car;
	int TCstart = TC;
	o head = _cons(_eval(((o)form)->car, env), NULL);
	//don't gc the result list
	gc_stack = _cons(head, gc_stack);
	o tail = head;
	form = ((o)form)->cdr;
	int nargs = 0;
	while (NULL != form) {
		o e = ((o)form)->car;
		((void)e);
		//count number of arguments
		o obj = _cons(_eval(e, env), NULL);
		((o)tail)->cdr = obj;
		tail = obj;
		nargs = 1 + nargs;
		form = ((o)form)->cdr;
	}
	function = ((o)head)->car;
	args = ((o)head)->cdr;
	if (1 == ((cons_symbol*)function)->type) {
		//function of type symbol
		{
			uintgr name = ((cons_symbol*)function)->name;
			if (37 <= name) {
				//name is not a bultin
				_putsn("(not a function)", 16);
				_putsn("EXIT\n", 6);
				exit(0);
			}
			if (nargs < lookupmin(name)) {
				_putsn("(too few args)", 14);
				_putsn("EXIT\n", 6);
				exit(0);
			}
			if (lookupmax(name) < nargs) {
				_putsn("(too many args)", 15);
				_putsn("EXIT\n", 6);
				exit(0);
			}
			o result = ((fn_ptr_type)lookupfn(name))(args, env);
			gc_stack = ((o)gc_stack)->cdr;
			//symbol
			return result;
		}
	}
	if (((2 != ((cons_symbol*)function)->type) && (1 != ((cons_symbol*)function)->type)) && issymbol(((o)function)->car, 2)) {
		//listp function and (car function) is lambda
		form = closure(TCstart, fname, NULL, ((o)function)->cdr, args, &env);
		gc_stack = ((o)gc_stack)->cdr;
		TC = 1;
		goto EVALJUMP;
	}
	if (((2 != ((cons_symbol*)function)->type) && (1 != ((cons_symbol*)function)->type)) && issymbol(((o)function)->car, 4)) {
		//listp function and (car function) is closure
		function = ((o)function)->cdr;
		form = closure(TCstart, fname, ((o)function)->car, ((o)function)->cdr, args, &env);
		gc_stack = ((o)gc_stack)->cdr;
		TC = 1;
		goto EVALJUMP;
	}
	_putsn("(illegal func)", 14);
	_putsn("EXIT\n", 6);
	exit(0);
	//eval returns nil
	return NULL;
}

o cdrx(o arg)
{
	if (0 == ((2 != ((cons_symbol*)arg)->type) && (1 != ((cons_symbol*)arg)->type))) {
		_putsn("(can't take cdr)", 16);
		_putsn("EXIT\n", 6);
		exit(0);
	}
	if (NULL == arg) {
		return NULL;
	}
	return ((o)arg)->cdr;
}

o carx(o arg)
{
	if (0 == ((2 != ((cons_symbol*)arg)->type) && (1 != ((cons_symbol*)arg)->type))) {
		_putsn("(can't take car)", 16);
		_putsn("EXIT\n", 6);
		exit(0);
	}
	if (NULL == arg) {
		return NULL;
	}
	return ((o)arg)->car;
}

o _apply(o function, o args, o *env)
{
	if (1 == ((cons_symbol*)function)->type) {
		uintgr name = ((cons_symbol*)function)->name;
		int nargs = listlength(args);
		if (37 < name) {
			_putsn("(not a function)", 16);
			_putsn("EXIT\n", 6);
			exit(0);
		}
		if (nargs < lookupmin(name)) {
			_putsn("(too few args)", 14);
			_putsn("EXIT\n", 6);
			exit(0);
		}
		if (lookupmin(name) < nargs) {
			_putsn("(too many args)", 15);
			_putsn("EXIT\n", 6);
			exit(0);
		}
		return ((fn_ptr_type)lookupfn(name))(args, *env);
	}
	if (((2 != ((cons_symbol*)function)->type) && (1 != ((cons_symbol*)function)->type)) && issymbol(((o)function)->car, 2)) {
		function = ((o)function)->cdr;
		o result = closure(0, NULL, NULL, function, args, env);
		return _eval(result, *env);
	}
	if (((2 != ((cons_symbol*)function)->type) && (1 != ((cons_symbol*)function)->type)) && issymbol(((o)function)->car, 4)) {
		function = ((o)function)->cdr;
		{
			o result = closure(0, NULL, ((o)function)->car, ((o)function)->cdr, args, env);
			return _eval(result, *env);
		}
	}
	_putsn("(illegal function)", 18);
	_putsn("EXIT\n", 6);
	exit(0);
	return NULL;
}

fn_ptr_type lookupfn(uintgr idx)
{
	return builtin_fptr[idx];
}

int lookupmax(uintgr idx)
{
	(void) name;
	return builtin_par_max[idx];
}

int lookupmin(uintgr idx)
{
	(void) name;
	return builtin_par_min[idx];
}

int builtin(char *name)
{
	intgr entry = 0;
	while (entry < 37) {
		if (0 == strcmp(name, builtin_name[entry])) {
			return entry;
		}
		entry = 1 + entry;
	}
	return 37;
}

int _string_eq_p(const char *a, const char *b, int n)
{
	for(int i = 0; (i < n) && a[i] && b[i]; i = 1 + i){
		if (a[i] != b[i]) {
			return 0;
		}
	}
	return 1;
}

int listlength(o list)
{
	int len = 0;
	while (NULL != list) {
		o e = ((o)list)->car;
		((void)e);
		len = 1 + len;
		list = ((o)list)->cdr;
	}
	return len;
}

o closure(int tail, o fname, o state, o function, o args, o *env)
{
	(void) fname;
	o params = ((o)function)->car;
	function = ((o)function)->cdr;
	//push state if not already in env
	while (NULL != state) {
		o pair = ((o)state)->car;
		((void)pair);
		if (NULL == findtwin(((o)pair)->car, *env)) {
			*env = _cons(pair, *env);
		}
		state = ((o)state)->cdr;
	}
	//add arguments to environment
	while ((NULL != params) && (NULL != args)) {
		o var = ((o)params)->car;
		o value = ((o)args)->car;
		if (tail) {
			{
				o pair = findtwin(var, *env);
				if (NULL != pair) {
					((o)pair)->cdr = value;
				}
				else {
					*env = _cons(_cons(var, value), *env);
				}
			}
		}
		else {
			*env = _cons(_cons(var, value), *env);
		}
		params = ((o)params)->cdr;
		args = ((o)args)->cdr;
	}
	if (NULL != params) {
		_putsn("(too few params)", 16);
		_putsn("EXIT\n", 6);
		exit(0);
	}
	if (NULL != args) {
		_putsn("(too many params)", 17);
		_putsn("EXIT\n", 6);
		exit(0);
	}
	//do implicit progn
	return tf_progn(function, *env);
}

o findtwin(o var, o env)
{
	while (NULL != env) {
		o item = ((o)env)->car;
		((void)item);
		if (var == ((o)item)->car) {
			return item;
		}
		env = ((o)env)->cdr;
	}
	return NULL;
}

o findvalue(o var, o env)
{
	uintgr varname = ((cons_symbol*)var)->name;
	o pair = value(varname, env);
	if (NULL == pair) {
		pair = value(varname, global_env);
	}
	if (NULL == pair) {
		_putsn("(unknown var)", 13);
		_putsn("EXIT\n", 6);
		exit(0);
	}
	return pair;
}

o value(uintgr n, o env)
{
	while (NULL != env) {
		o item = ((o)env)->car;
		((void)item);
		if (n == ((cons_symbol*)((o)item)->car)->name) {
			return item;
		}
		env = ((o)env)->cdr;
	}
	return NULL;
}

int _eq(o a, o b)
{
	return (a == b) || ((1 == ((cons_symbol*)a)->type) && (1 == ((cons_symbol*)b)->type) && (((cons_symbol*)a)->name == ((cons_symbol*)b)->name)) || ((2 == ((cons_symbol*)a)->type) && (2 == ((cons_symbol*)b)->type) && (((cons_number*)a)->integer == ((cons_number*)b)->integer));
}

int issymbol(o obj, uintgr n)
{
	return (1 == ((cons_symbol*)obj)->type) && (n == ((cons_symbol*)obj)->name);
}

intgr _integer(o obj)
{
	if (2 != ((cons_symbol*)obj)->type) {
		_putsn("(not number)", 12);
		_putsn("EXIT\n", 6);
		exit(0);
	}
	return ((cons_number*)obj)->integer;
}

char *name(o obj)
{
	buffer[3] = ((char)0);
	if (1 != ((cons_symbol*)obj)->type) {
		_putsn("(name)", 6);
		_putsn("EXIT\n", 6);
		exit(0);
	}
	uintgr x = ((cons_symbol*)obj)->name;
	if (x < 37) {
		return lookupstring(x);
	}
	for(int n = 2; 0 <= n; n = n - 1){
		buffer[n] = fromradix40(x % 40);
		x = x / 40;
	}
	return buffer;
}

char *lookupstring(uintgr idx)
{
	for(int i = 0; i < 7; i = 1 + i){
		buffer[i] = builtin_name[idx][i];
	}
	return buffer;
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

uintgr pack40(char *c)
{
	return (40 * ((40 * toradix40(c[0])) + toradix40(c[1]))) + toradix40(c[2]);
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
	_putsn("(ill. char in sym)", 18);
	_putsn("EXIT\n", 6);
	exit(0);
	return 0;
}

void gc(o form, o env)
{
	mark_object(tee);
	mark_object(global_env);
	mark_object(gc_stack);
	mark_object(form);
	mark_object(env);
	sweep();
}

void sweep(void)
{
	freelist = 0;
	freespace = 0;
	for(int i = (16 * 1024) - 1; 0 <= i; i = i - 1){
		o obj = workspace + i;
		if (1 == (0 != (((uintgr)((o)obj)->car) & (__UINT64_C(1) << ((8 * sizeof(uintptr_t)) - 1))))) {
			((o)obj)->car = ((o)(((uintgr)((o)obj)->car) & ((__UINT64_C(1) << ((8 * sizeof(uintptr_t)) - 1)) - 1)));
		}
		else {
			((o)obj)->car = ((o)0);
			((o)obj)->cdr = freelist;
			freelist = obj;
			freespace = 1 + freespace;
		}
	}
}

void mark_object(o obj)
{
	if (0 == obj) {
		return;
	}
	if (0 != (((uintgr)((o)obj)->car) & (__UINT64_C(1) << ((8 * sizeof(uintptr_t)) - 1)))) {
		return;
	}
	o arg = ((o)obj)->car;
	intgr type = ((cons_symbol*)obj)->type;
	((o)obj)->car = ((o)(((uintgr)((o)obj)->car) | (__UINT64_C(1) << ((8 * sizeof(uintptr_t)) - 1))));
	if ((1 != type) && (2 != type)) {
		mark_object(arg);
		mark_object(((o)obj)->cdr);
	}
}

o _symbol(uintgr name)
{
	cons_symbol *ptr = ((cons_symbol*)_alloc());
	((cons_symbol*)ptr)->type = 1;
	((cons_symbol*)ptr)->name = name;
	return ((o)ptr);
}

o _cons(o arg1, o arg2)
{
	o ptr = ((o)_alloc());
	((o)ptr)->car = arg1;
	((o)ptr)->cdr = arg2;
	return ptr;
}

o _number(intgr n)
{
	cons_number *ptr = ((cons_number*)_alloc());
	((cons_symbol*)ptr)->type = 2;
	((cons_number*)ptr)->integer = n;
	return ((o)ptr);
}

o _alloc(void)
{
	if (0 == freespace) {
		_putsn("(No room)", 9);
		_putsn("EXIT\n", 6);
		exit(0);
	}
	o temp = freelist;
	freelist = ((o)freelist)->cdr;
	freespace = freespace - 1;
	return temp;
}

void init_workspace(void)
{
	freelist = 0;
	for(intgr i = (16 * 1024) - 1; 0 <= i; i = i - 1){
		o obj = workspace + i;
		((o)obj)->car = 0;
		((o)obj)->cdr = freelist;
		freelist = obj;
		freespace = 1 + freespace;
	}
}

int main(int argc, char **argv)
{
	(void) argc;
	(void) argv;
	init_workspace();
	init_env();
	repl(NULL);
	return 0;
}