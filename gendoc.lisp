;; this file is only required to regenerate the README.md. Load this
;; after native-usb.lisp and call update-markdown-readme whenever new
;; functions were added or documentation was changed.

;; alternatively, if you native-usb.asd is linked into a subfolder of
;; quicklisp/local-projects, this file should load in slime with C-c
;; C-k

(eval-when (:compile-toplevel)
  (ql:quickload :mgl-pax)
  (ql:quickload :ulisp))

(mgl-pax:define-package :ulisp-doc
    (:documentation "C-mera s-expressions to generate Ulisp C code")
  (:use #:cl #:mgl-pax))

(in-package :ulisp-doc)

(mgl-pax:defsection @intro-sec (:title "Intro")
  "## What is this?
 
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
 ## Size

 ```
 [martin@localhost c-mera-ulisp]$ size b
    text    data     bss     dec     hex filename
  726051    7540   11704  745295   b5f4f b

 [martin@localhost c-mera-ulisp]$ nm -a --demangle --print-size --size-sort -t d b|tail -n 5 
 0000000004477152 0000000000011113 T vfprintf
 0000000004849056 0000000000013800 R __tens
 0000000004786272 0000000000019188 r translit_to_tbl
 0000000004635280 0000000000030365 T _IO_vfscanf
 0000000004635280 0000000000030365 T _IO_vfscanf_internal
 [martin@localhost c-mera-ulisp]$ readelf -s b|sort -nk 3|tail -n 5
  1986: 00000000004450e0 11113 FUNC    GLOBAL DEFAULT    6 vfprintf
  1650: 000000000049fda0 13800 OBJECT  GLOBAL DEFAULT   10 __tens
   431: 0000000000490860 19188 OBJECT  LOCAL  DEFAULT   10 translit_to_tbl
  1234: 000000000046ba90 30365 FUNC    GLOBAL DEFAULT    6 _IO_vfscanf_internal
  1760: 000000000046ba90 30365 FUNC    GLOBAL DEFAULT    6 _IO_vfscanf
 ```
 Show glibc dependencies https://insanecoding.blogspot.de/2012/07/creating-portable-linux-binaries.html
 ```
 [martin@localhost c-mera-ulisp]$ objdump -T ulisp-interp |grep GLIBC
 0000000000000000      DF *UND*  0000000000000000  GLIBC_2.2.5 write
 0000000000000000      DF *UND*  0000000000000000  GLIBC_2.2.5 read
 0000000000000000      DF *UND*  0000000000000000  GLIBC_2.2.5 __libc_start_main
 0000000000000000      DF *UND*  0000000000000000  GLIBC_2.3   __ctype_b_loc

 ```")


(mgl-pax:defsection @usage-sec (:title "Usage")
  ""
  (cg-user::gen-builtin-forward-declaration macro)
  (cg-user::gen-builtin-code macro)
  (cg-user::calc-builtin-name-max-len function)
  (cg-user::calc-builtin-name-list function)
  (cg-user::gen-builtin-strings macro)
  (cg-user::calc-builtin-fptr-list function)
  (cg-user::gen-builtin-fptr-clists macro)
  (cg-user::gen-builtin-min-max-clists macro)
  (cg-user::%dolist macro)
  (cg-user::%dolist2 macro)
  (cg-user::is-idx-in-type-range macro)
  )

(mgl-pax:defsection @ulisp-manual (:title "Native USB manual")
  ""
  (@intro-sec section)
  (@usage-sec section))




(defun update-markdown-readme ()
 (with-open-file (s (merge-pathnames "quicklisp/local-projects/c-mera-ulisp/README.md" (user-homedir-pathname))
		    :direction :output
		    :if-exists :supersede
		    :if-does-not-exist :create)
   (mgl-pax:document (list @ulisp-manual) :stream s :format :markdown
    )))

#+nil
(update-markdown-readme)
