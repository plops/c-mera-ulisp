# C-MERA-ULISP

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
repl _print_object read_rest nextitem _getc init_env _eval cdrx
carx _apply lookupfn lookupmax lookupmin builtin listlength closure
findtwin findvalue value _eq issymbol _integer name lookupstring
digitvalue pack40 fromradix40 toradix40 gc sweep mark_object _symbol
_cons _number _alloc erro init_workspace _read _eval
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
  (comment "(void) env;" :prefix "")
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
const char builtin_normalfunc_name[9][5] = { "add", "apply", "cdr", "car", "eq", "listp", "atom", "cons", "not" };
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