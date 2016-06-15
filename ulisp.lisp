(ql:quickload :cgen)
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
	(cast 'cons_object*
	      (\| (cast 'uintgr (cons-car ,x))
		  ;; #x8000
		  *mark-bit*
		  ))))

(defmacro unmark (x)
  `(set (cons-car ,x)
	(cast 'cons_object*
	      (& (cast 'uintgr (cons-car ,x))
		 ;; #x7fff
		 (- *mark-bit* 1)
		 ))))

(defmacro err (&rest rest))

(defparameter *none* 0)
(defparameter *symbol* 1)
(defparameter *number* 2)

(defmacro cons-car (x)
  `(pref (cast '(struct cons_object*) ,x) car))
(defmacro cons-cdr (x)
  `(pref (cast '(struct cons_object*) ,x) cdr))
(defmacro cons-name (x)
  `(pref (cast '(struct cons_symbol*) ,x) name))
(defmacro cons-type (x)
  `(pref (cast '(struct cons_symbol*) ,x) type))
(defmacro cons-integer (x)
  `(pref (cast '(struct cons_number*) ,x) integer))

(defmacro _second (x)
  `(cons-car (cons-cdr ,x)))

(defmacro _push (x y)
  `(set ,y (funcall _cons ,x ,y)))

(defmacro _pop (y)
  `(set ,y (cons-cdr ,y)))

(defparameter *builtin-function*
  '((symbols)
    (nil)
    (tee)
    (lambda)
    (let)
    (letstar)
    (closure)
    (specfrms)
    (quote)
    (defun)
    (defvar)
    (setq)
    (loop)
    (push)
    (pop)
    (incf)
    (decf)
    (progn)
    (return)
    (if)
    (cond)
    (and)
    (or)
    (functions)
    (not)
    (cons)
    (atom)
    (listp)
    (apply)))

(defun builtin-function-name (x)
  (first x))

(defun builtin-function-name-to-number (name)
  (loop for i from 0 and e in *builtin-function*
   when (eql name (builtin-function-name e))
   return i))

(defun builtin-function-name-maxlength ()
 (reduce #'cl:max
	 (mapcar #'(lambda (x) (cl:length (format nil "~a" (builtin-function-name x))))
		 *builtin-function*)))

(defmacro builtin-function-name-clist ()
  `(clist ,@(mapcar #'(lambda (x) (format nil "~a" (builtin-function-name x)))
		    *builtin-function*)))

#+nil
(builtin-function-name-clist)

(defmacro gen-builtin-table-string ()
  `(decl ,(loop for (e) in *builtin-function* and i from 0 collect
	       `(const char
		       ,(cl:intern (cl:format nil "STRING~3,'0d" i))
		       ,(cl:format nil "~a" e)))))

(defmacro gen-builtin-table-string-variables ()
  `(use-variables ,@(loop for (e) in *builtin-function* and i from 0 collect
			 (cl:intern (cl:format nil "STRING~3,'0d" i)))))

(defmacro defspecial (name &body body)
  `(function ,(intern (format nil "SP_~a" name)) ((cons_object* args)
						  (cons_object* env))
       -> cons_object*
     ,@body))

(defmacro ensure-symbol (var)
  `(if (!= *symbol* (cons-type ,var))
       (erro "not a symbol")))

#+nil
(let ((workspace-size 315)
      (buflen (builtin-function-name-maxlength)) ;; length of longest symbol 
      (cnil 'NULL))
  (with-open-file (*standard-output* "ulisp.c"
				    :direction :output
				    :if-exists :supersede
				    :if-does-not-exist :create)
   (loop for e in (list
		   (include <setjmp.h>)
		   (include <stdio.h>)
		   (include <stdint.h>)
		   (comment "I use integers that have the same size as a pointer")
		   (typedef uintptr_t uintgr)
		   (typedef intptr_t intgr)
		   
		   (comment "C-mera doesn't support unions")
		   (deftstruct cons_object
		     (decl ((cons_object* car)
			    (cons_object* cdr))))
		   (deftstruct cons_symbol
		     (decl ((uintgr type)
			    (uintgr name))))
		   (deftstruct cons_number
		     (decl ((uintgr type) 
			    (intgr integer))))
		   (decl ((const char (aref builtin-name
					(cl:length *builtin-function*)
					buflen)
				 (builtin-function-name-clist))))
		   (decl ((cons_object* freelist)
			  (cons_object* tee)
			  (cons_object* global-env)
			  (cons_object* gc-stack)
			  (uintgr freespace)
			  (cons_object (aref workspace workspace-size))
			  (jmp_buf exception)
			  (char (aref buffer (+ buflen 1)))))
		   (use-variables freelist
				  freespace
				  workspace
				  tee global-env gc-stack
				  exception
				  buffer
				  UINTPTR_MAX
				  builtin-name
				  NULL)
		   (function init-workspace () -> void
		     (set freelist 0)
		     (for ((intgr i (- workspace-size 1))
			   (<= 0 i) --i)
		       (decl ((struct cons_object* obj (+ workspace i)))
			 (set (cons-car obj) 0)
			 (set (cons-cdr obj) freelist)
			 (set freelist obj)
			 freespace++)))
		   (function erro ((const char* string)) -> void
		     (funcall printf "Error: %s\\n" string)
		     (set gc-stack 0)
		     (funcall longjmp exception 1))
		   (function _alloc () -> cons_object*
		     (if (== 0 freespace)
			 (funcall erro "No room"))
		     (decl ((cons_object* temp freelist))
		       (set freelist (cons-cdr freelist))
		       freespace--
		       (return temp)))
		   (function myfree ((cons_object* obj)) -> void
		     (set (cons-cdr obj) freelist)
		     (set freelist obj)
		     freespace++)
		   (function  _number ((intgr n)) -> cons_object*
		     (decl ((cons_number* ptr
					  (cast 
					   'cons_number*
					   (funcall _alloc))))
		       (set (pref ptr type) *number*)
		       (set (pref ptr integer) n)
		       (return (cast cons_object* ptr))))
		   (function _cons ((cons_object* arg1)
					(cons_object* arg2)) -> cons_object*
		     (decl ((cons_object* ptr (cast 'cons_object*
						    (funcall _alloc))))
		       (set (pref ptr car) arg1)
		       (set (pref ptr cdr) arg2)
		       (return ptr)))
		   (function _symbol ((uintgr name)) -> cons_object*
		       (decl ((cons_symbol* ptr
					  (cast 
					   'cons_symbol*
					   (funcall _alloc))))
			 (set (pref ptr type) *symbol*)
			 (set (pref ptr name) name)
			 (return (cast cons_object* ptr))))
 		   (function mark-object ((cons_object* obj)) -> void
		       (if (== 0 obj)
			   (return))
		       (if (marked obj)
			     (return))
		       (decl ((cons_object* arg (cons-car obj))
			      (intgr type (pref (cast 'cons_number* obj)
						type)))
			 (mark obj)
			 (if (and (!= *symbol* type)
				  (!= *number* type))
			     (progn
			       (funcall mark-object arg)
			       (funcall mark-object (cons-cdr obj))))))
		   (function sweep () -> void
		     (set freelist 0)
		     (set freespace 0)
		     (for ((int i (- workspace-size 1)) (<= 0 i) --i)
		       (decl ((cons_object* obj (+ workspace i)))
			 (if (== 1 (marked obj))
			     (unmark obj)
			     (progn
			       (set (cons-car obj) (cast 'cons_object* 0))
			       (set (cons-cdr obj) freelist)
			       (set freelist obj)
			       freespace++)))))
		   (function gc ((cons_object* form) (cons_object* env)) -> void
		     (mark-object tee)
		     (mark-object global-env)
		     (mark-object gc-stack)
		     (mark-object form)
		     (mark-object env)
		     (funcall sweep))
		   
		   (function toradix40 ((intgr ch)) -> intgr
		     (if (== 0 ch)
			 (return 0))
		     (if (and (<= #\0 ch) (<= ch #\9))
			 (return (+ 30 (- ch #\0))))
		     (set ch (\| ch #x20))
		     (if (and (<= #\a ch) (<= ch #\z))
			 (return (+ 1 (- ch #\a))))
		     (funcall erro "ill. char in sym")
		     (return 0))
		   (function fromradix40 ((intgr n)) -> intgr
		     (if (and (<= 1 n) (<= n 26))
			 (return (+ n #\a -1)))
		     (if (and (<= 1 30) (<= n 39))
			 (return (+ n #\0 -30)))
		     (if (== 27 n)
			 (return #\-))
		     (return 0))
		   (function pack40 ((char* c)) -> uintgr
		     (return (+ (* 40 (+ (* 40 (toradix40 (aref c 0)))
					 (toradix40 (aref c 1))))
				(toradix40 (aref c 2)))))
		   (function digitvalue ((char d)) -> intgr
		     (if (and (<= #\0 d)
			      (<= d #\9))
			 (return (- d #\0)))
		     (set d (\| d #x20))
		     (if (and (<= #\a d)
			      (<= d #\f))
			 (return (+ 10 (- d #\a ))))
		     (return 16))
		   (function lookupstring ((uintgr name)) -> char*
		     (for ((int i 0) (< i buflen) ++i)
		       (set (aref buffer i) (aref builtin-name name i)))
		     (return buffer))
		   (function name ((cons_object* obj)) -> char*
		     (set (aref buffer 3) (cast 'char 0))
		     (if (!= *symbol* (cons-type obj))
			 (erro "name"))
		     (decl ((uintgr x (cons-name obj)))
		       (if (< x (cl:length *builtin-function*))
			   (return (funcall lookupstring x)))
		       (for ((int n 2) (<= 0 n) --n)
			 (set (aref buffer n) (funcall fromradix40 (% x 40)))
			 (set x (/ x 40))))
		     (return buffer))
		   (function _integer ((cons_object* obj)) -> intgr
		       (if (!= *number* (cons-type obj))
			   (erro "not number"))
		       (return (cons-integer obj)))
		   (function issymbol ((cons_object* obj) (uintgr n)) -> int
		       (return (&& (== *symbol* (cons-type obj))
				   (cons-name n))))
		   (function _eq ((cons_object* a) (cons_object* b)) -> int
		     (return (\|\|
			      (== a b)
			      (&& (== *symbol* (cons-type a))
				  (== *symbol* (cons-type b))
				  (== (cons-name a) (cons-name b)))
			      (&& (== *number* (cons-type a))
				  (== *number* (cons-type b))
				  (== (cons-integer a) (cons-integer b))))))
		   (function value ((uintgr n) (cons_object* env)) -> cons_object*
		     (while (!= NULL env)
		       (decl ((cons_object* item (cons-car env)))
			 (if (== n (cons-name (cons-car item)))
			     (return item))
			 (set env (cons-cdr env))))
		     (return cnil))
		   (function findvalue ((cons_object* var) (cons_object* env)) -> cons_object*
		       (decl ((uintgr varname (cons-name var))
			      (cons_object* pair (funcall value varname env)))
			 (if (== NULL pair)
			     (set pair (funcall value varname global-env)))
			 (if (== NULL pair)
			     (erro "unknown var"))
			 (return pair)))
		   (function findtwin ((cons_object* var) (cons_object* env)) -> cons_object*
		     (while (!= NULL env)
		       (decl ((cons_object* item (cons-car env)))
			 (if (== var (cons-car item))
			     (return item))
			 (set env (cons-cdr env))))
		     (return cnil))
		   (function closure ((int tail)
				      (cons_object* fname)
				      (cons_object* state)
				      (cons_object* function)
				      (cons_object* args)
				      (cons_object** env)) -> cons_object*
		     (decl ((cons_object* params (cons-car function)))
		       (set function (cons-cdr function))
		       (comment "push state if not already in env")
		       (while (!= NULL state)
			 (decl ((cons_object *item (cons-car state)))
			   (if (== NULL (funcall findtwin (cons-car item)
						 *env))
			       (_push item *env))
			   (set state (cons-cdr state))))
		       (comment "add arguments to environment")
		       (while (&& (!= NULL params)
				  (!= NULL args))
			 (decl ((object_cons* var (cons-car params))
				(object_cons* value (cons-car args)))
			   (if tail
			       (decl ((cons_object* item (funcall findtwin var *env)))
				 (if (!= NULL item)
				     (set (cons-cdr item) value)
				     (_push (funcall _cons var value) *env)))
			       (_push (funcall _cons var value) *env))
			   (set params (cons-cdr params))
			   (set args (cons-cdr args))))
		       (if (!= NULL params)
			   (erro "too few params"))
		       (if (!= NULL args)
			   (erro "too many params"))
		       (comment "do implicit progn")
		       (return (funcall tf-progn function *env))))
		   (function listlength ((cons_object* list)) -> int
		     (decl ((int len 0))
		       (while (!= NULL list)
			 (set list (cons-cdr list))
			 len++)
		       (return len)))
		   
		   (comment "typedef object *(*fn_ptr_type)(object *, object *);" :prefix "")
		   (function _apply ((cons_object* function)
				    (cons_object* args)
				    (cons_object** env)) -> cons_object*
		     (if (== *symbol* (cons-type function))
			 (decl ((uintgr name (cons-name function))
				(int nargs (funcall listlength args)))
			   (if (<= (cl:length *builtin-function*) name)
			       (erro "not a function"))
			   (if (< nargs (funcall lookupmin name))
			       (erro "too few args"))
			   (if (< (funcall lookupmin name) nargs)
			       (erro "too many args"))
			   (return (funcall
				    (cast 'fn_ptr_type
					  (funcall lookupfn name))
				    args *env))))
		     (if (and (funcall listp function)
			      (funcall issymbol (cons-car function)
				       (builtin-function-name-to-number
					'lambda)))
			 (progn
			   (set function (cons-cdr function))
			   (decl ((cons_object*
				   result
				   (funcall closure 0
					    NULL NULL function args env)))
			     (return (funcall _eval result *env)))))
		     (if (and (funcall listp function)
			      (funcall issymbol (cons-car function)
				       (builtin-function-name-to-number
					'closure)))
			 (progn
			   (set function (cons-cdr function))
			   (decl ((cons_object*
				   result
				   (funcall closure 0
					    NULL
					    (cons-car function)
					    (cons-cdr function)
					    args env)))
			     (return (funcall _eval result *env)))))
		     (erro "illegal function")
		     (return NULL))
		   (comment "checked car and cdr")
		   (function carx ((cons_object* arg)) -> cons_object*
		     (if (== 0 (funcall listp arg))
			 (erro "can't take car"))
		     (if (== cnil arg)
			 (return cnil))
		     (return (cons-car arg)))
		   (function cdrx ((cons_object* arg)) -> cons_object*
		     (if (== 0 (funcall listp arg))
			 (erro "can't take cdr"))
		     (if (== cnil arg)
			 (return cnil))
		     (return (cons-cdr arg)))
		   (defspecial quote
		     (comment "(void) env;" :prefix "")
		     (return (cons-car args)))
		   (defspecial defun
		     (comment "(void) env;" :prefix "")
		     (decl ((cons_object* var (cons-car args)))
		       (ensure-symbol var)
		       (decl ((cons_object*
			       val
			       (funcall
				_cons
				(funcall
				 _symbol
				 (builtin-function-name-to-number 'lambda))
				(cons-cdr args)))
			      (cons_object* pair
					    (funcall value (cons-name var)
						     global-env)))
			 (if (!= NULL pair)
			     (progn (set (cons-cdr pair) val)
				    (return var)))
			 (_push (funcall _cons var val) global-env)
			 (return var))))
		   (defspecial defvar
		     (decl ((cons_object* var (cons-car args)))
		       (ensure-symbol var)
		       (decl ((cons_object* val (funcall _eval (_second args)
							 env))
			      (cons_object* pair (funcall value (cons-name var)
							  global-env)))
			 (if (!= NULL pair)
			     (set (cons-cdr pair) val)
			     (return var))
			 (_push (funcall _cons var val)
				  global-env)
			 (return var))))
		   (defspecial setq
		     (decl ((cons_object* arg (funcall _eval
						       (_second args)
						       env))
			    (cons_object* pair (funcall findvalue
							(cons-car args)
							env)))
		       (set (cons-cdr pair) arg)
		       (return arg)))
		   (function main () -> int
		    
		     (return 0))) 
      do
	(simple-print e))))



