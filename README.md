<a id='x-28ULISP-DOC-3A-40ULISP-MANUAL-20MGL-PAX-3ASECTION-29'></a>

# Native USB manual

## Table of Contents

- [1 Usage][797d]
- [2 Usage][ab00]

###### \[in package ULISP-DOC\]


<a id='x-28ULISP-DOC-3A-40INTRO-SEC-20MGL-PAX-3ASECTION-29'></a>

## 1 Usage

## What is this?

I started from ulisp 1.0 (later 1.1) and tried to implement as few
functions I deemed necessary. I express all the code in c-mera, which
is an s-expression based representation that can be processed with
Common Lisp macros and expanded into C code.  The functions can be
separated in to three types of builtins (special, tail recursive and
normal functions):

```
special: def incf pop push loop setq defvar defun quote
tailrec: or and cond if return progn
normalfunc: add apply cdr car eq listp atom cons not
```

I believe these functions should be enough to create some interesting
programs.

A few more functions are required to implement the runtime. I call
those boiler plate functions. These are:

```
init_env init_workspace _alloc
_symbol _cons _number  _integer
gc sweep mark_object

_getc nextitem _read read_rest  

repl _print_object erro name lookupstring
pack40 fromradix40 toradix40

_apply _eval

closure
 
cdrx carx listlength issymbol _eq

lookupfn builtin lookupmax lookupmin 

findtwin findvalue value digitvalue 
```

Note that I prefixed some functions with an underscore `_` to avoid
name clashes with Common Lisp functions.

My motivation for expressing the code with c-mera are to
learn about the ulisp implementation, without having to touch C.

I already see that Common Lisp macros allow me to express C functions
in a more lispy way. Lets look at the implementation of the function
`add`:

```common-lisp
(deffunction (add 0 127)
  (comment '(void) env;' :prefix '')
  (decl ((intgr result 0))
    (%dolist (item args)
      (decl ((intgr temp (funcall _integer item)))
        (set result (+ result temp))))
    (return (funcall _number result))))
```

These c-mera expressions expand into the following C code:

```C
o fn_add(o args, o env)
{
        //minimum number of parameters: 0, max. nr. of parameters: 127
        (void) env;
        intgr result = 0;
        while (NULL != args) {
                o item = ((o)args)->car;
                intgr temp = _integer(item);
                result = result + temp;
                args = ((o)args)->cdr;
        }
        return _number(result);
}
```

Let's have a look at the macro `%dolist`. In the first implementation
I expand into a while loop with a `,@body` expansion in the middle and
the cdr propagation at the end. Later I added a hygienic variable
using `gensym` to ensure that the `list` argument is only evaluated
once (not shown here). 

```
(defmacro %dolist ((item list) &body body)
  `(while (!= NULL ,list)
     (decl ((o ,item (cons-car ,list)))
       ,@body)
     (set ,list (cons-cdr ,list))))
```

The macro `deffunction` stores the name, the code, the maximum and
minimum number of expected arguments, and the function prototype in a
data structure. Therefore the function is only defined in one place
and all required code can be generated automatically. Here is the
forward declaration for the function `fn_add`, an entry `fn_add` in
the function pointer table, a string `add` in the function name table
and the entries for the number of arguments in corresponding tables:

```
o fn_add(o args, o env);
fn_ptr_type builtin_normalfunc_fptr[9] = { fn_add, fn_apply, fn_cdr, fn_car, fn_eq, fn_listp, fn_atom, fn_cons, fn_not };
const char builtin_normalfunc_name[9][5] = { 'add', 'apply', 'cdr', 'car', 'eq', 'listp', 'atom', 'cons', 'not' };
const char builtin_normalfunc_par_min[9] = { 0, 2, 1, 1, 2, 1, 1, 2, 1 };
const char builtin_normalfunc_par_max[9] = { 127, 127, 1, 1, 2, 1, 1, 2, 1 };
```

Eventually, I might add dependency tracking to functions, so that one
can compile a Lisp image that only includes the required functions
(i.e. everything required to provide I2C but none of the SPI
functionality). Ideally I would like to get something running on a
Lattice FPGA with icestorm.

## Intermediate result:

One thing I learned is that the style of ulisp is not type safe. It is
very easy to confuse a number object for an object of symbol type. It
think the best approach to ensure functional correctness is to
implement test cases for as many of the functions as possible.

Another issue with Lisp, that I haven't really thought about before is
real-time performance. The current code does not seem to be
deterministic. When a function is added to the internal symbol table,
lookup-times can change. I want to ensure that functions run without
jitter in deterministic time.

## Goals:

Compared to the original ulisp I changed the way symbol strings are
stored. Perhaps I can use a hash based method to ensure cycle exact
name lookups in constant time.

Currently I write docstrings for the boiler plate functions. Because
these are the hardest to get my head around. Eventually, I hope to
export this documentation using pax-mgl.

I hope to understand ulisp enough to port it to a variety of
microcontrollers, everytime I have to use some microcontroller,
really.

I would like to investigate the feasibility of implementing a swank
interface, so that I can communicate with the Lisp using Emacs/SLIME.

How to make this run on a Lattice iCE40HX-8K FPGA?

## Hacking

I'm trying to store name strings in 4 arrays. I will have to change a
few functions, which currently assume all symbols to be present in one
array. These functions are:

```
builtin string -> idx
lookupstring idx -> string
name obj -> char*buffer
lookupmin idx -> int
lookupmax idx -> int
lookupfn  idx -> fptr
init_env nextitem builtin-function-name-to-number symbol -> idx
_eval apply

```


<a id='x-28ULISP-DOC-3A-40USAGE-SEC-20MGL-PAX-3ASECTION-29'></a>

## 2 Usage



<a id='x-28CG-USER-3A-3AGEN-BUILTIN-FORWARD-DECLARATION-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **cg-user::gen-builtin-forward-declaration** 

    Generate forward declarations for all the functions in the C file.

<a id='x-28CG-USER-3A-3AGEN-BUILTIN-CODE-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **cg-user::gen-builtin-code** 

    Emit the code for all the functions.

<a id='x-28CG-USER-3A-3ACALC-BUILTIN-NAME-MAX-LEN-20FUNCTION-29'></a>

- [function] **cg-user::calc-builtin-name-max-len** *L*

    Find the longest function name. The name doesn't include the
    prefix.

<a id='x-28CG-USER-3A-3ACALC-BUILTIN-NAME-LIST-20FUNCTION-29'></a>

- [function] **cg-user::calc-builtin-name-list** *L*

    Generate a list of function names (without prefix) like
    this: ('decf' 'incf' 'pop' 'push' 'loop' 'setq' 'defvar' 'defun'
    'quote')

<a id='x-28CG-USER-3A-3AGEN-BUILTIN-STRINGS-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **cg-user::gen-builtin-strings** 

    Create string array like this:
    const char builtin\_name[53][7] = { 'nil', 'tee', 'lambda', 'let',
    'closure', 'decf', 'incf', 'pop', 'push', 'loop', 'setq', 'defvar',
    'defun', 'quote', 'decf', 'incf', 'pop', 'push', 'loop', 'setq',
    'defvar', 'defun', 'quote', 'or', 'and', 'cond', 'if', 'return',
    'progn', 'or', 'and', 'cond', 'if', 'return', 'progn', 'add', 'apply',
    'cdr', 'car', 'eq', 'listp', 'atom', 'cons', 'not', 'add', 'apply',
    'cdr', 'car', 'eq', 'listp', 'atom', 'cons', 'not' };

<a id='x-28CG-USER-3A-3ACALC-BUILTIN-FPTR-LIST-20FUNCTION-29'></a>

- [function] **cg-user::calc-builtin-fptr-list** *L*

    PREFIX is sp*, tf* or fn\_. L is the global list with the functions,
    e.g. *builtin-special*. Example output: (sp\_decf sp\_incf sp\_pop
    sp\_push sp\_loop sp\_setq sp\_defvar sp\_defun sp\_quote)

<a id='x-28CG-USER-3A-3AGEN-BUILTIN-FPTR-CLISTS-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **cg-user::gen-builtin-fptr-clists** 

    Create array of function pointers, like this:
    fn\_ptr\_type builtin\_fptr[53][] = { 0, 0, 0, 0, 0, sp\_decf, sp\_incf,
    sp\_pop, sp\_push, sp\_loop, sp\_setq, sp\_defvar, sp\_defun, sp\_quote,
    sp\_decf, sp\_incf, sp\_pop, sp\_push, sp\_loop, sp\_setq, sp\_defvar,
    sp\_defun, sp\_quote, tf\_or, tf\_and, tf\_cond, tf\_if, tf\_return,
    tf\_progn, tf\_or, tf\_and, tf\_cond, tf\_if, tf\_return, tf\_progn, fn\_add,
    fn\_apply, fn\_cdr, fn\_car, fn\_eq, fn\_listp, fn\_atom, fn\_cons, fn\_not,
    fn\_add, fn\_apply, fn\_cdr, fn\_car, fn\_eq, fn\_listp, fn\_atom, fn\_cons,
    fn\_not };
    Note that the symbols (the first 5 elements) have null pointers.

<a id='x-28CG-USER-3A-3AGEN-BUILTIN-MIN-MAX-CLISTS-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **cg-user::gen-builtin-min-max-clists** 

    Emit C code to define arrays containing the min and max number of
    arguments for the functions. Example output:
    
    const char builtin\_par\_max[53][] = { 0, 0, 0, 0, 0, 2, 2, 1, 2, 127, 2,
    127, 127, 1, 2, 2, 1, 2, 127, 2, 127, 127, 1, 127, 127, 127, 3, 127,
    127, 127, 127, 127, 3, 127, 127, 127, 127, 1, 1, 2, 1, 1, 2, 1, 127,
    127, 1, 1, 2, 1, 1, 2, 1 };
    
    const char builtin\_par\_min[53][] = { 0, 0, 0, 0, 0, 1, 1, 1, 2, 0, 2, 0,
    0, 1, 1, 1, 1, 2, 0, 2, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 0, 0, 2, 0, 0,
    0, 2, 1, 1, 2, 1, 1, 2, 1, 0, 2, 1, 1, 2, 1, 1, 2, 1 };

<a id='x-28CG-USER-3A-3A-25DOLIST-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **cg-user::%dolist** *(ITEM LIST) &body BODY*

    The list can be an expression like (cons-cdr bla). It will only be evaluated once.

<a id='x-28CG-USER-3A-3A-25DOLIST2-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **cg-user::%dolist2** *((E1 L1) (E2 L2)) &body BODY*

    Go through two lists simultaneously

<a id='x-28CG-USER-3A-3AIS-IDX-IN-TYPE-RANGE-20-28MGL-PAX-3AMACRO-29-29'></a>

- [macro] **cg-user::is-idx-in-type-range** *IDX TYPE*

    This emits a range check to test whether an index IDX into
    *builtin-declaration* and therefore the builtin function name table is
    of TYPE symbol, special, tailrec or normalfunc.
    Example: (is-idx-in-type-range i special) => (and (<= 5 i) (<= i 22))

  [797d]: #x-28ULISP-DOC-3A-40INTRO-SEC-20MGL-PAX-3ASECTION-29 "Usage"
  [ab00]: #x-28ULISP-DOC-3A-40USAGE-SEC-20MGL-PAX-3ASECTION-29 "Usage"
