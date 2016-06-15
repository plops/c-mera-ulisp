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

(defmacro _third (x)
  `(cons-car (cons-cdr (cons-cdr ,x))))

(defmacro _push (x y)
  `(set ,y (funcall _cons ,x ,y)))

(defmacro _pop (y)
  `(set ,y (cons-cdr ,y)))

(defmacro _listp (x)
  `(and (!= *number* (cons-type ,x))
	(!= *symbol* (cons-type ,x))))

(defmacro _consp (x)
  `(and (!= *number* (cons-type ,x))
	(!= *symbol* (cons-type ,x))
	(!= NULL ,x)))



(defparameter *builtin-function*
  '((f_sym)
    (nil)
    (tee)
    (lambda)
    (let)
    (closure)
    (f_spec)
    (quote)
    (defun)
    (defvar)
    (setq)
    (loop)
    (push)
    (pop)
    (incf)
    (decf)
    (f_tail)
    (progn)
    (return)
    (if)
    (cond)
    (and)
    (or)
    (f_fun)
    (not)
    (cons)
    (atom)
    (listp)
    (eq)
    (car)
    (cdr)
    (apply)))

;; (let ((a 1)
;;       (b 2))
;;   (+ a b))
;; (car '(+ a b))
;; (cdr '(+ a b))

(defun builtin-function-name (x)
  (first x))

(defun builtin-function-name-to-number (name)
  (loop for i from 0 and e in *builtin-function*
   when (eql name (builtin-function-name e))
   return i))

(defun builtin-symbol-list ()
  (loop for i from (cl:+ 1 (builtin-function-name-to-number 'f_sym))
     below (builtin-function-name-to-number 'f_spec)
     collect (first (elt *builtin-function* i))))

(defun builtin-special-function-list ()
  (loop for i from (cl:+ 1 (builtin-function-name-to-number 'f_spec))
     below (builtin-function-name-to-number 'f_tail)
     collect (first (elt *builtin-function* i))))

(defun builtin-tail-function-list ()
  (loop for i from (cl:+ 1 (builtin-function-name-to-number 'f_tail))
     below (builtin-function-name-to-number 'f_fun)
     collect (first (elt *builtin-function* i))))

(defun builtin-normal-function-list ()
  (loop for i from (cl:+ 1 (builtin-function-name-to-number 'f_fun))
     below (cl:length *builtin-function*)
     collect (first (elt *builtin-function* i))))

(defun builtin-function-name-type (name)
  "find in which class NAME is. It can be either :sym, :spec, :tail
or :fun. Returns nil for the delimeters F_SYM F_SPEC, F_TAIL and F_FUN
and throws error when string is not a builtin."
  (unless (member name (mapcar #'first *builtin-function*))
    (error "~a not a builtin" name))
  (cl:cond ((member name (builtin-symbol-list)) :sym)
	   ((member name (builtin-special-function-list)) :spec)
	   ((member name (builtin-tail-function-list)) :tail)
	   ((member name (builtin-normal-function-list)) :fun)))

(defmacro builtin-function-ptr-clist ()
  `(clist ,@(mapcar #'(lambda (x) (case (builtin-function-name-type x)
				    (:spec (intern (string-upcase (format nil "sp_~a" x))))
				    (:tail (intern (string-upcase (format nil "tf_~a" x))))
				    (:fun (intern (string-upcase (format nil "fn_~a" x))))
				    (t 0)))
		    (mapcar #'first *builtin-function*))))

#+nil
(builtin-function-ptr-clist)

#+nil
(builtin-function-name-type 'not)

#+nil
(builtin-normal-function-list)

(defmacro builtin-symbol ()
  `(builtin-function-name-to-number 'f_sym))
(defmacro builtin-special ()
  `(builtin-function-name-to-number 'f_spec))
(defmacro builtin-tail ()
  `(builtin-function-name-to-number 'f_tail))
(defmacro builtin-function ()
  `(builtin-function-name-to-number 'f_fun))

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
  `(function ,(intern (string-upcase (format nil "sp_~a" name)))
       ((cons_object* args)
	(cons_object* env))
       -> cons_object*
     ,@body))

(defmacro deftailrec (name &body body)
  `(function ,(intern (string-upcase (format nil "tf_~a" name))) ((cons_object* args)
								  (cons_object* env))
       -> cons_object*
     ,@body))

(defmacro deffunction (name &body body)
  `(function ,(intern (string-upcase (format nil "fn_~a" name))) ((cons_object* args)
								  (cons_object* env))
       -> cons_object*
     ,@body))

(defmacro ensure-symbol (var)
  `(if (!= *symbol* (cons-type ,var))
       (erro "not a symbol")))

(defmacro inc (x)
  `(set ,x (+ 1 ,x)))
(defmacro dec (x)
  `(set ,x (- ,x 1)))

(defmacro %dolist (e &body body)
  `(while (!= NULL ,e)
     ,@body
     (set ,e (cons-cdr ,e))))

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
		    (comment "typedef cons_object *(*fn_ptr_type)(cons_object *, cons_object *);" :prefix "")
		    (deftstruct cons_symbol
		      (decl ((uintgr type)
			     (uintgr name))))
		    (deftstruct cons_number
		      (decl ((uintgr type) 
			     (intgr integer))))
		    (typedef cons_object* o)
		    (decl ((const char (aref builtin-name
					 (cl:length *builtin-function*)
					 buflen)
				  (builtin-function-name-clist))
			   ))
		    (decl ((cons_object* freelist)
			   (cons_object* tee)
			   (cons_object* global-env)
			   (cons_object* gc-stack)
			   (uintgr freespace)
			   (cons_object (aref workspace workspace-size))
			   (jmp_buf exception)
			   (char return-flag 0)
			   (char (aref buffer (+ buflen 1)))
			   ))
		    
		    (use-variables freelist
				   freespace
				   workspace
				   tee global-env gc-stack
				   exception
				   buffer
				   UINTPTR_MAX
				   builtin-name
				   builtin-fptr
				   return-flag
				   NULL
				   EVAL)
		    (comment "forward declarations")
		    (deftailrec progn) (comment ";" :prefix "")
		    (function _eval ((cons_object* form)(cons_object* env)) -> cons_object*) (comment ";" :prefix "")
		    (function init-workspace () -> void
		      (set freelist 0)
		      (for ((intgr i (- workspace-size 1))
			    (<= 0 i) (dec i))
			(decl ((struct cons_object* obj (+ workspace i)))
			  (set (cons-car obj) 0)
			  (set (cons-cdr obj) freelist)
			  (set freelist obj)
			  (inc freespace))))
		    (function erro ((const char* string)) -> void
		      (funcall printf "Error: %s\\n" string)
		      (set gc-stack 0)
		      (funcall longjmp exception 1))
		    (function _alloc () -> cons_object*
		      (if (== 0 freespace)
			  (funcall erro "No room"))
		      (decl ((cons_object* temp freelist))
			(set freelist (cons-cdr freelist))
			(set freespace (- freespace 1))
			(return temp)))
		    (function myfree ((cons_object* obj)) -> void
		      (set (cons-cdr obj) freelist)
		      (set freelist obj)
		      (inc freespace))
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
		      (for ((int i (- workspace-size 1)) (<= 0 i) (dec i))
			(decl ((cons_object* obj (+ workspace i)))
			  (if (== 1 (marked obj))
			      (unmark obj)
			      (progn
				(set (cons-car obj) (cast 'cons_object* 0))
				(set (cons-cdr obj) freelist)
				(set freelist obj)
				(inc freespace))))))
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
		      (for ((int i 0) (< i buflen) (inc i))
			(set (aref buffer i) (aref builtin-name name i)))
		      (return buffer))
		    (function name ((cons_object* obj)) -> char*
		      (set (aref buffer 3) (cast 'char 0))
		      (if (!= *symbol* (cons-type obj))
			  (erro "name"))
		      (decl ((uintgr x (cons-name obj)))
			(if (< x (builtin-function))
			    (return (funcall lookupstring x)))
			(for ((int n 2) (<= 0 n) (dec n))
			  (set (aref buffer n) (funcall fromradix40 (% x 40)))
			  (set x (/ x 40))))
		      (return buffer))
		    (function _integer ((cons_object* obj)) -> intgr
		      (if (!= *number* (cons-type obj))
			  (erro "not number"))
		      (return (cons-integer obj)))
		    (function issymbol ((cons_object* obj) (uintgr n)) -> int
		      (return (and (== *symbol* (cons-type obj))
				   (cons-name n))))
		    (function _eq ((cons_object* a) (cons_object* b)) -> int
		      (return (or
				(== a b)
				(and (== *symbol* (cons-type a))
				     (== *symbol* (cons-type b))
				     (== (cons-name a) (cons-name b)))
				(and (== *number* (cons-type a))
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
		      (comment "(void) fname;" :prefix "")
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
			(while (and (!= NULL params)
				    (!= NULL args))
			  (decl ((o var (cons-car params))
				 (o value (cons-car args)))
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
			  (inc len))
			(return len)))
		    (function lookupmin ((uintgr name)) -> int
		      (comment "(void) name;" :prefix "")
		      (return 0))
		    (function lookupmax ((uintgr name)) -> int
		      (comment "(void) name;" :prefix "")
		      (return 3))
		    (decl ((fn_ptr_type
			   (aref builtin-fptr (cl:length *builtin-function*)))))
		    (function lookupfn ((uintgr name)) -> fn_ptr_type
		      (return (aref builtin-fptr name)))
		    (function _apply ((cons_object* function)
				      (cons_object* args)
				      (cons_object** env)) -> cons_object*
		      (if (== *symbol* (cons-type function))
			  (decl ((uintgr name (cons-name function))
				 (int nargs (funcall listlength args)))
			    (if (<= (builtin-function) name)
				(erro "not a function"))
			    (if (< nargs (funcall lookupmin name))
				(erro "too few args"))
			    (if (< (funcall lookupmin name) nargs)
				(erro "too many args"))
			    (return (funcall
				     (cast 'fn_ptr_type
					   (funcall lookupfn name))
				     args *env))))
		      (if (and (_listp function)
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
		      (if (and (_listp function)
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
		      (if (== 0 (_listp arg))
			  (erro "can't take car"))
		      (if (== cnil arg)
			  (return cnil))
		      (return (cons-car arg)))
		    (function cdrx ((cons_object* arg)) -> cons_object*
		      (if (== 0 (_listp arg))
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
		    (defspecial loop
		      (set return-flag 0)
		      (decl ((cons_object* start args))
			(for (() () ())
			  (set args start)
			  (while (!= NULL args)
			    (decl ((cons_object* form (cons-car args))
				   (cons_object* result (funcall _eval form env)))
			      (if (== 1 return-flag)
				  (set return-flag 0)
				  (return result)))
			    (set args (cons-cdr args))))))
		    (defspecial push
		      (decl ((cons_object* item (funcall _eval (cons-car args) env))
			     (cons_object* pair (funcall findvalue (_second args)
							 env)))
			(_push item (cons-cdr pair))
			(return (cons-cdr pair))))
		    (defspecial pop
		      (decl ((cons_object* pair (funcall findvalue (_second args)
							 env))
			     (cons_object* result (cons-car (cons-cdr pair))))
			(_pop (cons-cdr pair))
			(return result)))
		    (defspecial incf
		      (decl ((cons_object* var (cons-car args))
		    	     (cons_object* pair (funcall findvalue var env))
		    	     (int result (funcall _integer (funcall _eval var env)))
		    	     (int temp 1))
		    	(if (!= NULL (cons-cdr args))
		    	    (set temp (funcall _integer (funcall _eval (_second args) env))))
		    	(set result (+ result temp))
		    	(set var (funcall _number result))
		    	(set (cons-cdr pair) var)
		    	(return var)))
		    (defspecial decf
		      (decl ((cons_object* var (cons-car args))
		    	     (cons_object* pair (funcall findvalue var env))
		    	     (int result (funcall _integer (funcall _eval var env)))
		    	     (int temp 1))
		    	(if (!= NULL (cons-cdr args))
		    	    (set temp (funcall _integer
					       (funcall _eval (_second args) env))))
		    	(set result (- result temp))
		    	(set var (funcall _number result))
		    	(set (cons-cdr pair) var)
		    	(return var)))
		    (deftailrec progn
		      (if (== NULL args)
		    	  (return cnil))
		      (decl ((cons_object* more (cons-cdr args)))
		    	(while (!= NULL more)
		    	  (funcall _eval (cons-car args) env)
		    	  (set args more)
		    	  (set more (cons-cdr args)))
		    	(return (cons-car args))))
		    (deftailrec return
		      (set return-flag 1)
		      (return (funcall tf_progn args env)))
		    (deftailrec if
		      (if (!= cnil (funcall _eval (cons-car args) env))
			  (return (_second args)))
		      (return (_third args)))
		    (deftailrec cond
		      (while (!= NULL args)
			(decl ((o clause (cons-car args))
			       (o test (funcall _eval (cons-car clause) env))
			       (o forms (cons-cdr clause)))
			  (if (!= cnil test)
			      (if (== NULL forms)
				  (return test)
				  (return (funcall tf_progn forms env)))))
			(set args (cons-cdr args)))
		      (return cnil))
		    (deftailrec and
		      (if (== NULL args)
			  (return tee))
		      (decl ((o more (cons-cdr args)))
			(while (!= NULL more)
			  (if (== NULL (funcall _eval (cons-car args) env))
			      (return cnil))
			  (set args more)
			  (set more (cons-cdr args)))
			(return (cons-car args))))
		    (deftailrec or
		      (decl ((o more (cons-cdr args)))
			(while (!= NULL more)
			  (decl ((o result (funcall _eval (cons-car args) env)))
			    (if (!= NULL result)
			       (return result)))
			  (set args more)
			  (set more (cons-cdr args)))
			(return (cons-car args))))
		    (deffunction not
		      (comment "(void) env;" :prefix "")
		      (if (== cnil (cons-car args))
			  (return tee)
			  (return cnil)))
		    (deffunction cons
		      (comment "(void) env;" :prefix "")
		      (return (funcall _cons (cons-car args)
				       (_second args))))
		    (deffunction atom
		      (comment "(void) env;" :prefix "")
		      (decl ((o arg1 (cons-car args)))
			(if (_consp arg1)
			    (return cnil)
			    (return tee))))
		    (deffunction listp
		      (comment "(void) env;" :prefix "")
		      (decl ((o arg1 (cons-car args)))
			(if (_listp arg1)
			    (return tee)
			    (return cnil))))
		    (deffunction eq
		      (comment "(void) env;" :prefix "")
		      (decl ((o arg1 (cons-car args))
			     (o arg2 (_second args)))
			(if (funcall _eq arg1 arg2)
			    (return tee)
			    (return cnil))))
		    (deffunction car
		      (comment "(void) env;" :prefix "")
		      (return (funcall carx (cons-car args))))
		    (deffunction cdr
		      (comment "(void) env;" :prefix "")
		      (return (funcall cdrx (cons-car args))))
		    (deffunction apply
		      (decl ((o previous NULL)
			     (o last args))
			(while (!= NULL (cons-cdr last))
			  (set previous last)
			  (set last (cons-cdr last)))
			(if (_listp (cons-car last))
			    (progn (set (cons-cdr previous) (cons-car last))
				   (return (funcall _apply (cons-car args)
						    (cons-cdr args)
						    (addr-of env))))
			    (erro "last arg not list"))))
		    
		    (function _eval ((cons_object* form)
				     (cons_object* env)) -> cons_object*
		      (decl ((int TC 0))
			(comment "EVAL:" :prefix "") ;; FIXME this is crazy
			(if (< freespace 10)
			    (funcall gc form env))
			;; FIXME i left out some _end stuff and serial break
			(if (== NULL form)
			    (return cnil))
			(if (== *number* (cons-type form))
			    (return form))
			(if (== *symbol* (cons-type form))
			    (decl ((uintgr name (cons-name form)))
			      (if (== (builtin-function-name-to-number 'nil)
				      name)
				  (return cnil))
			      (decl ((cons_object* pair (funcall value name env)))
				(if (!= NULL pair)
				    (return (cons-cdr pair)))
				(set pair (value name global-env))
				(if (!= NULL pair)
				    (return (cons-cdr pair))
				    (if (<= name (builtin-function))
					(return form))))
			      (erro "undefined")))
			(comment "it's a list")
			(decl ((cons_object* function (cons-car form))
			       (cons_object* args (cons-cdr form)))
			  (comment "list starting with symbol?")
			  (if (== *symbol* (cons-type function))
			      (decl ((uintgr name (cons-name function)))
				(comment "process LET")
				(if (== (builtin-function-name-to-number 'let)
					name)
				    ;; FIXME leaving out LETSTAR
				    
				    (decl ((cons_object* assigns (cons-car args))
					   (cons_object* forms (cons-cdr args))
					   (cons_object* newenv env))
				      (while (!= NULL assigns)
					(decl ((cons_object* assign (cons-car assigns)))
					  (if (_consp assign)
					      (_push (_cons (cons-car assign)
							    (funcall _eval
								     (_second assign)
								     env))
						     newenv)
					      (_push (_cons assign cnil)
						     newenv)))
					(set assigns (cons-cdr assigns)))
				      (set env newenv)
				      (set form (funcall tf_progn forms env))
				      (set TC 1)
				      (comment "goto EVAL;" :prefix "")))
				(comment "process LAMBDA")
				(if (== (builtin-function-name-to-number 'lambda) name)
				    (progn
				      (if (== NULL env)
					  (return form))
				      (decl ((cons_object* envcopy NULL))
					(while (!= NULL env)
					  (decl ((cons_object* pair (cons-car env))
						 (cons_object* val (cons-cdr pair)))
					    (if (== *number* (cons-type val))
						(set val (funcall _number
								  (cons-integer val))))
					    (_push (funcall _cons (cons-car pair)
							    val)
						   envcopy)
					    (set env (cons-cdr env))))
					(return (funcall
						 _cons
						 (funcall
						  _symbol
						  (builtin-function-name-to-number
						   'closure))
						 (funcall _cons envcopy args))))))
				
				(if (and (< (builtin-function-name-to-number 'f_spec) name)
					 (< name (builtin-function-name-to-number 'f_tail)))
				    (progn
				      (comment "process SPECIAL form")
				      (return (funcall (cast 'fn_ptr_type (funcall lookupfn name))
						       args env))))
				(if (and (< (builtin-function-name-to-number 'f_tail) name)
					 (< name (builtin-function-name-to-number 'f_fun)))
				    (progn
				      (comment "process TAIL CALL form")
				      (set form (funcall (cast 'fn_ptr_type (funcall lookupfn name))
							 args env))
				      (set TC 1)
				      (comment "goto EVAL;" :prefix "")))))
			  (comment "evaluate the parameters - result in head")
			  (decl ((cons_object* fname (cons-car form))
				 (int TCstart TC)
				 (cons_object* head (funcall _cons
							     (funcall _eval
								      (cons-car form)
								      env)
							     NULL)))
			    (comment "don't gc the result list")
			    (_push head gc-stack) 
			    (decl ((cons_object* tail head))
			      (set form (cons-cdr head))
			      (decl ((int nargs 0))
				(while (!= NULL form)
				  (decl ((cons_object* obj (funcall _cons
								    (funcall _eval (cons-car form) env)
								    NULL)))
				    (set (cons-cdr tail) obj)
				    (set tail obj)
				    (set form (cons-cdr form))
				    (inc nargs)))
				(set function (cons-car head))
				(set args (cons-cdr head))
				(if (== *symbol* (cons-type function))
				    (decl ((uintgr name (cons-name function)))
				      (if (<= (cl:length *builtin-function*) name)
					  (err "not a function"))
				      (if (< nargs (funcall lookupmin name))
					  (err "too few args"))
				      (if (< (funcall lookupmax name) nargs)
					  (err "too many args"))
				      (decl ((o result (funcall
							(cast 'fn_ptr_type
							      (funcall lookupfn name))
							args env)))
					(_pop gc-stack)
					(return result))))
				(if (and (_listp function)
					 (funcall issymbol (cons-car function)
						  (builtin-function-name-to-number 'lambda)))
				    (progn
				     (set form (funcall closure TCstart fname NULL
							(cons-cdr function) args (addr-of env)))
				     (_pop gc-stack)
				     (set TC 1)
				     (comment "goto EVAL;" :prefix "")))
				(if (and (_listp function)
					 (funcall issymbol (cons-car function)
						  (builtin-function-name-to-number 'closure)))
				    (progn
				      (set function (cons-cdr function))
				      (set form (funcall closure TCstart (cons-car function)
							 (cons-cdr function) args (addr-of env)))
				     (_pop gc-stack)
				     (set TC 1)
				     (comment "goto EVAL;" :prefix "")))
				(erro "illegal func")
				(return cnil)))))))
		    (function initenv () -> void
		      (set global-env NULL)
		      (set tee (funcall _symbol (builtin-function-name-to-number 'tee))))
		    (decl ((fn_ptr_type
			   (aref builtin-fptr (cl:length *builtin-function*))
					(builtin-function-ptr-clist)
					)))
		    (function main () -> int
		      (funcall initworkspace)
		      (funcall initenv)
		      (return 0))) 
       do
	 (simple-print e))))



