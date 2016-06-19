# C-MERA-ULISP

## What is this?

I started from ulisp 1.0 (later 1.1) and tried to implement as few functions I deemed necessary in c-mera.
These functions can be separated in to three types of builtins (special, tail recursive and normal functions):


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

The macro `deffunction` stores the 

## Intermediate result:

One thing I learned is that the style of ulisp is not type safe. It is
very easy to confuse a number object for an object of symbol type. It
think the best approach to ensure functional correctness is to
implement a test cases for many of the functions.



## Goals:

I changed the way symbol strings are stored. Perhaps I can use a hash
based method to ensure cycle exact name lookups. Currently I write
docstrings for the boiler plate functions. Because these are the
hardest to get my head around. Eventually I hope to export this
documentation using pax-mgl.

Eventually, I hope to understand ulisp enough to port it to a variety
of microcontrollers.  I would like to investigate the feasibility of
implementing a swank interface, so that I can communicate with the
Lisp using Emacs/SLIME.
