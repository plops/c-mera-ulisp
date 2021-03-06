(eval-when (:compile-toplevel) (ql:quickload :cgen))
(in-package :cg-user)
(switch-reader)

;; 32 kBytes flash
;;  - look up table with built-in function names
;; 2 kByte RAM
;;  - user defined symbols three letters/digits in 16bit word

;; signed integer arithmetic in single word (2 bytes)
;; every lisp object a two word cell (4 bytes)
;;  - symbol (1 "cat) , number (2 -42) or cons (. .)
;;  - garbage collection marks objects by setting top bit of leftmost word
;;    (never more than 32 kBytes of RAM)

;; look at the data section:
;; objdump -s -j .rodata ulisp.o

(defmacro deftstruct (name &body body)
  `(progn
     (typedef struct ,name ,name)
     (struct ,name
       ,@body)))


(defparameter *mark-bit*
  ;; #x8000
  ;; #x8000000000000000
  (<< (funcall __UINT64_C 1) (- (* 8 (sizeof uintptr_t)) 1)))

(defmacro marked (x)
  `(!= 0
       (& (cast 'uintgr (cons-car ,x))
	  *mark-bit*)))

(defmacro mark (x)
  `(set (cons-car ,x)
	(cast 'o
	      (\| (cast 'uintgr (cons-car ,x))
		  ;; #x8000
		  *mark-bit*
		  ))))

(defmacro unmark (x)
  `(set (cons-car ,x)
	(cast 'o
	      (& (cast 'uintgr (cons-car ,x))
		 ;; #x7fff
		 (- *mark-bit* 1) 
		 ))))

(defmacro %err (&rest rest)
  `(progn (%puts ,(cl:substitute #\space #\newline (cl:format nil "~a" rest)))
	  (%puts "EXIT\\n")
	  (funcall exit 0)
	  ))

(defparameter *none* 0)
(defparameter *symbol* 1)
(defparameter *number* 2)

(defmacro cons-car (x)
  `(pref (cast 'o ,x) car))
(defmacro cons-cdr (x)
  `(pref (cast 'o ,x) cdr))
(defmacro cons-name (x)
  `(pref (cast '(struct cons_symbol*) ,x) name))
(defmacro cons-type (x)
  `(pref (cast '(struct cons_symbol*) ,x) type))
(defmacro cons-integer (x)
  `(pref (cast '(struct cons_number*) ,x) integer))

(defmacro %second (x)
  `(cons-car (cons-cdr ,x)))

(defmacro %third (x)
  `(cons-car (cons-cdr (cons-cdr ,x))))

(defmacro %push (x y)
  `(set ,y (funcall _cons ,x ,y)))

(defmacro %pop (y)
  `(set ,y (cons-cdr ,y)))

(defmacro %listp (x)
  `(and (!= *number* (cons-type ,x))
	(!= *symbol* (cons-type ,x))))

(defmacro %consp (x)
  `(and (!= *number* (cons-type ,x))
	(!= *symbol* (cons-type ,x))
	(!= NULL ,x)))

(defparameter *boiler-func* nil)
(defparameter *builtin-symbol*
  '(((:name nil))
    ((:name tee))
    ((:name lambda))
    ((:name let))
    ((:name closure))))
(defparameter *builtin-special* nil)
(defparameter *builtin-tailrec* nil)
(defparameter *builtin-normalfunc* nil)

(defmacro dcomment (x)
  ;`(funcall printf "%s\\n",x)
  )
(defmacro %puts (x)
  `(funcall _putsn ,x ,(cl:length x))
  )

(defmacro def-with-prefix ((type name &optional (min 1) (max min)) &body body)
  "Generates macros to define special, tail-recursive and normal
functions that are then exported to C. The output data is stored in
global variables from where other macros later generate the forward
definitions, the C code and some string arrays."
  (let ((target (ecase type
		  (sp '*builtin-special*)
		  (tf '*builtin-tailrec*)
		  (fn '*builtin-normalfunc*)))
	(fwd `(function ,(intern (string-upcase (format nil "~a_~a" type name)))
		   ((o args)
		    (o env))
		   -> o))
	(code `(function ,(intern (string-upcase (format nil "~a_~a" type name)))
		   ((o args)
		    (o env))
		   -> o
		 (comment ,(format nil "minimum number of parameters: ~a, max. nr. of parameters: ~a" min max))
		 (dcomment ,(format nil "~a\\n" name))
		 ,@body)))
    `(cl:progn
       (cl:push '((:name ,name)
		  (:min ,min)
		  (:max ,max)
		  (:fwd ,fwd)
		  (:code ,code)) ,target))))

(defmacro defspecial ((name &optional (min 1) (max min)) &body body)
  `(def-with-prefix (sp ,name ,min ,max) ,@body))

(defmacro deftailrec ((name &optional (min 1) (max min)) &body body)
  `(def-with-prefix (tf ,name ,min ,max) ,@body))

(defmacro deffunction ((name &optional (min 1) (max min)) &body body)
  `(def-with-prefix (fn ,name ,min ,max)
     (comment "(void) env;" :prefix "") ;; every definition seems to have that
     ,@body))

(defun parameter-names (l)
    "turn a c-mera function parameter list with types like this ((o form) (o env)) into a list of variable names like this (form env)."
    (mapcar #'first (mapcar #'last l)))

;; (defmacro %function (name parameters -> type &body body)
;;   "This is a macro for defining the general helper functions. Push
;; name, forward declaration and function code into *boiler-func*. For
;; each function (except a few functions that are used for printing) two
;; functions are declared. One with the suffice _not_timed that
;; implements the functionality and another which calls this function
;; while measuring and printing the cpu cycles."
;;   (let ((name2 (intern (string-upcase (format nil "~a-not-timed" name))))
;; 	(start (intern (string-upcase (format nil "~a" (gensym "start"))))))
;;    `(cl:push '((:name ,name)
;; 	       (:fwd  (function ,name ,parameters ,-> ,type))
;; 	       (:code (progn
;; 		  (function ,name2 ,parameters -> ,type
;; 		    ,@body)
;; 		  (function ,name ,parameters -> ,type
;; 		    ,(cl:if (cl:member name '(_putsn putui _putchar puti _isspace _getchar digitvalue mark-object _string_eq_p)) 
;; 			    (cl:if (eq type 'void)
;; 				   `(funcall ,name2 ,@(parameter-names parameters))
;; 				   `(return (funcall ,name2 ,@(parameter-names parameters))))
;; 		    	    `(decl ((uintgr ,start (funcall __rdtsc)))
;; 		    	       ,(cl:if (eq type 'void)
;; 		    	    	       `(progn
;; 		    	    		  (funcall ,name2 ,@(parameter-names parameters))
;; 		    	    		  (%puts ,(format nil " ~a:" name))
;; 		    	    		  (funcall putui (- (funcall __rdtsc)
;; 		    	    				    ,start))
;; 		    	    		  (funcall _putchar #\Newline))
;; 		    	    	       `(decl ((,type ret (funcall ,name2 ,@(parameter-names parameters))))
;; 		    	    		  (%puts ,(format nil " ~a:" name))
;; 		    	    		  (funcall putui (- (funcall __rdtsc)
;; 		    	    				    ,start))
;; 		    	    		  (funcall _putchar #\Newline)
;; 		    	    		  (return ret)))))))))
;; 	       *boiler-func*)))
(defmacro %function (name parameters -> type &body body)
  "This is a macro for defining the general helper functions. Push
name, forward declaration and function code into *boiler-func*"
  `(cl:push '((:name ,name)
	      (:fwd  (function ,name ,parameters ,-> ,type))
	      (:code (progn
		       (function ,name ,parameters -> ,type
				 ;; ,(cl:unless (member name '(_putsn putui _putchar puti _isspace _getchar digitvalue mark-object _string_eq_p
				 ;; 			    _print_object issymbol name lookupstring _strlen princ))
				 ;; 	     `(%puts ,(format nil "~a\\n" name)))
			 ,@body))))
	    *boiler-func*))

(defun get-builtin-fwd (alist)
  (second (assoc :fwd alist)))
(defun get-builtin-max (alist)
  (second (assoc :max alist)))
(defun get-builtin-min (alist)
  (second (assoc :min alist)))
(defun get-builtin-name (alist)
  (second (assoc :name alist)))
(defun get-builtin-code (alist)
  (second (assoc :code alist)))
(defun get-builtin-type (alist)
  (second (assoc :type alist)))

(defmacro gen-builtin-forward-declaration ()
  "Generate forward declarations for all the functions in the C file."
  `(progn
     ,@(loop for e in (append *builtin-special*
			      *builtin-tailrec*
			      *builtin-normalfunc*
			      *boiler-func*) collect
	  `(progn ,(get-builtin-fwd e)
		  (comment ";" :prefix "")))))

(defmacro gen-builtin-code ()
  "Emit the code for all the functions."
  `(progn
     ,@(loop for e in
	    (append *builtin-special*
		    *builtin-tailrec*
		    *builtin-normalfunc*
		    *boiler-func*) collect
	  (get-builtin-code e))))



(defun calc-builtin-name-max-len (l)
  "Find the longest function name. The name doesn't include the
prefix."
  (reduce #'cl:max
	  (mapcar #'(lambda (x)
		      (cl:length (format nil "~a"
					 (get-builtin-name x))))
		  l)))

#+nil
(mapcar #'calc-builtin-name-max-len (list *builtin-special*
					  *builtin-tailrec*
					  *builtin-normalfunc*))

(defun calc-builtin-name-list (l)
  "Generate a list of function names (without prefix) like
this: ('decf' 'incf' 'pop' 'push' 'loop' 'setq' 'defvar' 'defun'
'quote')"
  (mapcar #'(lambda (x)
	      (format () "~a" (get-builtin-name x)))
	  l))

#+nil
(calc-builtin-name-list *builtin-declaration*)
#+nil
(calc-builtin-name-max-len *builtin-declaration*)

(defparameter *builtin-declaration* nil)

(defmacro gen-builtin-strings ()
"Create string array like this:
const char builtin_name[53][7] = { 'nil', 'tee', 'lambda', 'let',
'closure', 'decf', 'incf', 'pop', 'push', 'loop', 'setq', 'defvar',
'defun', 'quote', 'decf', 'incf', 'pop', 'push', 'loop', 'setq',
'defvar', 'defun', 'quote', 'or', 'and', 'cond', 'if', 'return',
'progn', 'or', 'and', 'cond', 'if', 'return', 'progn', 'add', 'apply',
'cdr', 'car', 'eq', 'listp', 'atom', 'cons', 'not', 'add', 'apply',
'cdr', 'car', 'eq', 'listp', 'atom', 'cons', 'not' };
"
  `(cl:progn
     (use-variables 
       builtin-name)
     (decl ((const char (aref builtin-name
			  ,(cl:length *builtin-declaration*)
			  ,(calc-builtin-name-max-len *builtin-declaration*))
		  (clist ,@(calc-builtin-name-list *builtin-declaration*)))))))

#+nil
(gen-builtin-strings)

(defun calc-builtin-fptr-list (l)
  "PREFIX is sp_, tf_ or fn_. L is the global list with the functions,
e.g. *builtin-special*. Example output: (sp_decf sp_incf sp_pop
sp_push sp_loop sp_setq sp_defvar sp_defun sp_quote)"
  (mapcar #'(lambda (x)
	      (let ((prefix (ecase (get-builtin-type x)
			      (symbol nil)
			      (special "sp_")
			      (tailrec "tf_")
			      (normalfunc "fn_"))))
		(cl:if prefix
		       (intern (string-upcase (format nil "~a~a" prefix (get-builtin-name x))))
		       0)))
	  l))

(defmacro gen-builtin-fptr-clists ()
  "Create array of function pointers, like this:
fn_ptr_type builtin_fptr[53] = { 0, 0, 0, 0, 0, sp_decf, sp_incf,
sp_pop, sp_push, sp_loop, sp_setq, sp_defvar, sp_defun, sp_quote,
sp_decf, sp_incf, sp_pop, sp_push, sp_loop, sp_setq, sp_defvar,
sp_defun, sp_quote, tf_or, tf_and, tf_cond, tf_if, tf_return,
tf_progn, tf_or, tf_and, tf_cond, tf_if, tf_return, tf_progn, fn_add,
fn_apply, fn_cdr, fn_car, fn_eq, fn_listp, fn_atom, fn_cons, fn_not,
fn_add, fn_apply, fn_cdr, fn_car, fn_eq, fn_listp, fn_atom, fn_cons,
fn_not };
Note that the symbols (the first 5 elements) have null pointers."
  `(cl:progn
     (use-variables
       builtin-fptr)
     (decl ((fn_ptr_type (aref builtin-fptr ,(cl:length *builtin-declaration*))
		  (clist ,@(calc-builtin-fptr-list *builtin-declaration*)))))))

#+nil
(gen-builtin-fptr-clists)

;; const uintgr builtin_par_max[33] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

(defun calc-builtin-min (l)
  (mapcar #'get-builtin-min l))

(defun calc-builtin-max (l)
  (mapcar #'get-builtin-max l))

(defmacro gen-builtin-min-max-clists ()
"Emit C code to define arrays containing the min and max number of
arguments for the functions. Example output:

const char builtin_par_max[53] = { 0, 0, 0, 0, 0, 2, 2, 1, 2, 127, 2,
127, 127, 1, 2, 2, 1, 2, 127, 2, 127, 127, 1, 127, 127, 127, 3, 127,
127, 127, 127, 127, 3, 127, 127, 127, 127, 1, 1, 2, 1, 1, 2, 1, 127,
127, 1, 1, 2, 1, 1, 2, 1 };

const char builtin_par_min[53] = { 0, 0, 0, 0, 0, 1, 1, 1, 2, 0, 2, 0,
0, 1, 1, 1, 1, 2, 0, 2, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 0, 0, 2, 0, 0,
0, 2, 1, 1, 2, 1, 1, 2, 1, 0, 2, 1, 1, 2, 1, 1, 2, 1 };
"
  `(cl:progn
     (use-variables
      builtin-par-min
      builtin-par-max)
     (decl ((const char (aref builtin-par-max (cl:length *builtin-declaration*))
		   (clist ,@(substitute 0 nil (calc-builtin-max *builtin-declaration*))))
	    (const char (aref builtin-par-min (cl:length *builtin-declaration*))
		   (clist ,@(substitute 0 nil (calc-builtin-min *builtin-declaration*))))))))



(defmacro ensure-symbol (var)
  `(if (!= *symbol* (cons-type ,var))
       (%err "not a symbol")))

(defmacro inc (x)
  `(set ,x (+ 1 ,x)))
(defmacro dec (x)
  `(set ,x (- ,x 1)))

(defmacro %dolist ((item list) &body body)
  "The list can be an expression like (cons-cdr bla). It will only be evaluated once."
  (cl:let ((e (intern (format nil "~a" (gensym)))))
    (cl:if (cl:listp list)
	   `(decl ((o ,e ,list))
	      
	   (while (!= NULL ,e)
	     (decl ((o ,item (cons-car ,e)))
	       (cast 'void ,item)
	       ,@body)
	     (set ,e (cons-cdr ,e))))
	`(while (!= NULL ,list)
	   (decl ((o ,item (cons-car ,list)))
	     (cast 'void ,item)
	     ,@body)
	   (set ,list (cons-cdr ,list))))))


(defmacro %dolist2 (((e1 l1) (e2 l2)) &body body)
  "Go through two lists simultaneously"
  `(while (and (!= NULL ,l1)
	       (!= NULL ,l2))
     (decl ((o ,e1 (cons-car ,l1))
	    (o ,e2 (cons-car ,l2)))
       ,@body)
     (set ,l1 (cons-cdr ,l1))
     (set ,l2 (cons-cdr ,l2))))


(defparameter *unused* 0)
(defparameter *bra* 1)
(defparameter *ket* 2)
(defparameter *quo* 3)
(defparameter *dot* 4)


(defmacro gen-cmd (cmd)
  (cl:let* ((cmd-s (substitute #\space #\newline (cl:format nil "~a" cmd))))
    `(decl ((char (aref cmd ,(cl:length cmd-s))
		  ,cmd-s)))))


(defmacro when (clause &body body)
  `(if ,clause
       (progn
	 ,@body)))

(defmacro %car (x)
  `(cons-car ,x))

(defmacro %cdr (x)
  `(cons-cdr ,x))

(use-variables freelist
	       freespace
	       workspace
	       tee global-env gc-stack
	       exception
	       buffer
	       UINTPTR_MAX
	       builtin-name
	       builtin-fptr
	       builtin-par-min
	       builtin-par-max
	       return-flag
	       NULL
	       EVALJUMP
	       EOF
	       last-char
	       ;;cmd
	       )


(progn 
  (load "special")
  (load "tailrec")
  (load "normfunc")
  (load "boiler")
  (defparameter *builtin-declaration*
    (append (mapcar #'(lambda (x) (push '(:type symbol) x)) *builtin-symbol*)
	    (mapcar #'(lambda (x) (push '(:type special) x)) (sort (copy-seq *builtin-special*) #'string<= :key #'caar))
	    (mapcar #'(lambda (x) (push '(:type tailrec) x)) (sort (copy-seq *builtin-tailrec*) #'string<= :key #'caar))
	    (mapcar #'(lambda (x) (push '(:type normalfunc) x)) (sort (copy-seq *builtin-normalfunc*) #'string<= :key #'caar)))))



(defmacro is-idx-in-type-range (idx type)
    "This emits a range check to test whether an index IDX into
*builtin-declaration* and therefore the builtin function name table is
of TYPE symbol, special, tailrec or normalfunc.
Example: (is-idx-in-type-range i special) => (and (<= 5 i) (<= i 22))"
    (declare (type (member symbol special tailrec normalfunc) type))
 (let ((start-idx 
	(position type
		  (mapcar #'(lambda (x) (second (assoc :type x)))
			  *builtin-declaration*)
		  :from-end nil))
       (end-idx (position type
			  (mapcar #'(lambda (x) (second (assoc :type x)))
				  *builtin-declaration*)
			  :from-end t)))
   `(and (<= ,start-idx ,idx)
	 (<= ,idx ,end-idx))))

(defun get-builtin-idx-from-name (symb)
  (cl:position symb (cl:mapcar #'get-builtin-name *builtin-declaration*)))

#+nil
(let ((workspace-size (* 128 1024 1024))
      (buflen  ;; length of longest symbol 
       (cl:max
	(calc-builtin-name-max-len *builtin-symbol*)
	(calc-builtin-name-max-len (append *builtin-normalfunc*
					   *builtin-special*
					   *builtin-tailrec*
					   ))))
      (cnil 'NULL))
  (with-open-file (*standard-output* "ulisp.c"
				     :direction :output
				     :if-exists :supersede
				     :if-does-not-exist :create)
    (loop for e in (list
		    (comment "Headers")
		    ;(include <setjmp.h>)
					;(include <stdio.h>)
		    (include <x86intrin.h>) (comment "__rdtsc intrinsic")
		    (include <stdint.h>) (comment "uintptr_t")
		    ;(include <ctype.h>)  (comment "isspace")
		    (include <stdlib.h>) (comment "exit")
		    (include <unistd.h>) (comment "write")
		    (include <string.h>) (comment "strcmp")

		    (comment "Type declarations")
		    (comment "I use integers that have the same size as a pointer")
		    (typedef uintptr_t uintgr)
		    (typedef intptr_t intgr)
		    
		    (comment "C-mera doesn't support unions")
		    (deftstruct cons_object
		      (decl ((cons_object* car)
			     (cons_object* cdr))))
		    (comment "typedef cons_object *(*fn_ptr_type)(cons_object *, cons_object *);" :prefix "")
		    (deftstruct cons_symbol
		      (decl ((uintgr type)
			     (uintgr name))))
		    (deftstruct cons_number
		      (decl ((uintgr type) 
			     (intgr integer))))
		    
		    (typedef cons_object* o)
		    (typedef unsigned char uchar)
		    
		    
		    (comment "#undef NULL" :prefix "")

		    (decl ((o freelist)
			   (o tee)
			   (o global-env)
			   (o gc-stack)
			   (uintgr freespace)
			   (cons_object (aref workspace workspace-size))
			   ;(jmp_buf exception)
			   (char return-flag 0)
			   (char (aref buffer (+ buflen 1)))
			   (char last-char)
			   (void* NULL 0)))
		    
		    (comment "forward declarations")
		    (gen-builtin-forward-declaration)
		    (comment "Global variables")
		    
		    (gen-builtin-strings)
		    (gen-builtin-min-max-clists)
		    
		    
		    (gen-builtin-fptr-clists)
		    
		    (gen-builtin-code)
		    
		    (function main ((int argc) (char** argv)) -> int
		      (comment "(void) argc;" :prefix "")
		      (comment "(void) argv;" :prefix "")
		      (when (!= *mark-bit*
				(funcall __UINT64_C #x8000000000000000))
			(%puts "error"))
		      (funcall init-workspace)
		      (funcall init-env)
		      (repl NULL)
		      (return 0))) 
       do
	 (simple-print e))))
