(eval-when (:compile-toplevel) (ql:quickload :cgen))
(in-package :cg-user)
;; (defpackage :g
;;   (:use :cgen))
;; (in-package :g)
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

(defmacro err (&rest rest)
  `(progn (funcall erro ,(cl:substitute #\space #\newline (cl:format nil "~a" rest)))
	  (funcall printf "EXIT\\n")
	  (funcall exit 0)))

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

(eval-when (:compile-toplevel)
  (defun reset-builtin ()
 (defparameter *builtin-function*
   '(((:name f_sym))
     ((:name nil))
     ((:name tee))
     ((:name lambda))
     ((:name let))
     ((:name closure))
     ((:name f_spec))
     ((:name quote))
     ((:name defun))
     ((:name defvar))
     ((:name setq))
     ((:name loop))
     ((:name push))
     ((:name pop))
     ((:name incf))
     ((:name decf))
     ((:name f_tail))
     ((:name progn))
     ((:name return))
     ((:name if))
     ((:name cond))
     ((:name and))
     ((:name or))
     ((:name f_fun))
     ((:name not))
     ((:name cons))
     ((:name atom))
     ((:name listp))
     ((:name eq))
     ((:name car))
     ((:name cdr))
     ((:name apply))
     ((:name add)))))
 (reset-builtin))

;; (defparameter *builtin-symbol*
;;   '(((:name nil))
;;     ((:name tee))
;;     ((:name lambda))
;;     ((:name let))
;;     ((:name closure))))

;; (defparameter *builin-special*
;;   '(((:max 1) (:min 1) (:name quote))
;;     ((:max 127) (:min 0) (:name defun))
;;     ((:max 127) (:min 0) (:name defvar))
;;     ((:max 2) (:min 2) (:name setq))
;;     ((:max 127) (:min 0) (:name loop))
;;     ((:max 2) (:min 2) (:name push))
;;     ((:max 1) (:min 1) (:name pop))
;;     ((:max 2) (:min 1) (:name incf))
;;     ((:max 2) (:min 1) (:name decf))))

;; (defparameter *builtin-tailrec* '(((:max 127) (:min 0) (:name progn))
;;   ((:max 127) (:min 0) (:name return))
;;   ((:max 3) (:min 2) (:name if))
;;   ((:max 127) (:min 0) (:name cond))
;;   ((:max 127) (:min 0) (:name and))
;;   ((:max 127) (:min 0) (:name or))))

;; (defparameter *builtin-normalfunc* 
;;   '(((:max 1) (:min 1) (:name not))
;;    ((:max 2) (:min 2) (:name cons))
;;    ((:max 1) (:min 1) (:name atom))
;;    ((:max 1) (:min 1) (:name listp))
;;    ((:max 2) (:min 2) (:name eq))
;;    ((:max 1) (:min 1) (:name car))
;;    ((:max 1) (:min 1) (:name cdr))
;;    ((:max 127) (:min 2) (:name apply))
;;    ((:max 127) (:min 0) (:name add))))

(defparameter *builtin-symbol* nil)
(defparameter *builtin-special* nil)
(defparameter *builtin-tailrec* nil)
(defparameter *builtin-normalfunc* nil)

(defun builtin-function-name (x)
  (second (assoc :name x)))

(defun builtin-function-max (x)
  (second (assoc :max x)))

(defun builtin-function-min (x)
  (second (assoc :min x)))

(defun builtin-function-name-to-number (name)
  (loop for i from 0 and e in *builtin-function*
   when (eql name (builtin-function-name e))
     return i))

#+nil
(builtin-function-name-to-number 'add)

(defun builtin-symbol-list ()
  (loop for i from (cl:+ 1 (builtin-function-name-to-number 'f_sym))
     below (builtin-function-name-to-number 'f_spec)
     collect (builtin-function-name (elt *builtin-function* i))))

(defun builtin-special-function-list ()
  (loop for i from (cl:+ 1 (builtin-function-name-to-number 'f_spec))
     below (builtin-function-name-to-number 'f_tail)
     collect (builtin-function-name (elt *builtin-function* i))))

(defun builtin-tail-function-list ()
  (loop for i from (cl:+ 1 (builtin-function-name-to-number 'f_tail))
     below (builtin-function-name-to-number 'f_fun)
     collect (builtin-function-name (elt *builtin-function* i))))

(defun builtin-normal-function-list ()
  (loop for i from (cl:+ 1 (builtin-function-name-to-number 'f_fun))
     below (cl:length *builtin-function*)
     collect (builtin-function-name (elt *builtin-function* i))))

(defun builtin-function-name-type (name)
  "find in which class NAME is. It can be either :sym, :spec, :tail
or :fun. Returns nil for the delimeters F_SYM F_SPEC, F_TAIL and F_FUN
and throws error when string is not a builtin."
  (unless (member name (mapcar #'builtin-function-name *builtin-function*))
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
		    (mapcar #'builtin-function-name *builtin-function*))))

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

(defmacro builtin-function-max-clist ()
  `(clist ,@(mapcar #'(lambda (x) (cl:let ((y (builtin-function-max x)))
				    (cl:if y y 0)))
		    *builtin-function*)))

(defmacro builtin-function-min-clist ()
  `(clist ,@(mapcar #'(lambda (x) (cl:let ((y (builtin-function-min x)))
				    (cl:if y y 0)))
		    *builtin-function*)))

#+nil
(builtin-function-name-clist)

(defmacro gen-builtin-table-string ()
  `(decl ,(loop for e in *builtin-function* and i from 0 collect
	       `(const char
		       ,(cl:intern (cl:format nil "STRING~3,'0d" i))
		       ,(cl:format nil "~a" (builtin-function-name e))))))

(defmacro gen-builtin-table-string-variables ()
  `(use-variables ,@(loop for e in *builtin-function* and i from 0 collect
			 (cl:intern (cl:format nil "STRING~3,'0d" i)))))





(defmacro deftailrec-fw (name)
  `(progn
     (function ,(intern (string-upcase (format nil "tf_~a" name))) ((o args)
									 (o env))
	      -> o)
	  (comment ";" :prefix "")))

(defmacro dcomment (x)
  ;`(funcall printf "%s\\n",x)
  )

(defmacro def-with-prefix ((type name &optional (min 1) (max min)) &body body)
  (let ((target (ecase type
		  (sp '*builtin-special*)
		  (tf '*builtin-tailrec*)
		  (fn '*builtin-normalfunc*)))
	(code `(function ,(intern (string-upcase (format nil "~a_~a" type name)))
		   ((o args)
		    (o env))
		   -> o
		 (comment ,(format nil "minimum number of parameters: ~a, max. nr. of parameters: ~a" min max))
		 (dcomment ,(format nil "~a\\n" name))
		 ,@body)))
    `(cl:progn
      (cl:progn
	(cl:push '(:min ,min) (cl:elt *builtin-function* (builtin-function-name-to-number ',name)))
	(cl:push '(:max ,max) (cl:elt *builtin-function* (builtin-function-name-to-number ',name)))
	(cl:push '(:type ,type) (cl:elt *builtin-function* (builtin-function-name-to-number ',name)))
	(loop for e in '((:code ,code) (:min ,min) (:max ,max) (:name ,name)) do
	     (cl:push e ,target))))))

#+nil
(deftailrec (progn 0 127)
		      (when (== NULL args)
			(return cnil))
		      (decl ((o more (%cdr args)))
			(%dolist (e more)
			  (funcall _eval (%car args) env)
			  (set args more))
			
			(return (%car args))))

(defmacro defspecial ((name &optional (min 1) (max min)) &body body)
  `(def-with-prefix (sp ,name ,min ,max) ,@body))

(defmacro deftailrec ((name &optional (min 1) (max min)) &body body)
  `(def-with-prefix (tf ,name ,min ,max) ,@body))

(defmacro deffunction ((name &optional (min 1) (max min)) &body body)
  `(def-with-prefix (fn ,name ,min ,max) ,@body))

(defmacro ensure-symbol (var)
  `(if (!= *symbol* (cons-type ,var))
       (erro "not a symbol")))

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
	       ,@body)
	     (set ,e (cons-cdr ,e))))
	`(while (!= NULL ,list)
	   (decl ((o ,item (cons-car ,list)))
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

#+nil
*builtin-function*

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


#+nil
(let ((workspace-size 315)
      (buflen (builtin-function-name-maxlength)) ;; length of longest symbol 
      (cnil 'NULL))
  ;; (reset-builtin)
  (with-open-file (*standard-output* "ulisp.c"
				     :direction :output
				     :if-exists :supersede
				     :if-does-not-exist :create)
    (loop for e in (list
		    (include <setjmp.h>)
		    (include <stdio.h>)
		    (include <stdint.h>) ;; uintptr_t
		    (include <ctype.h>)  ;; isschar
		    (include <stdlib.h>) ;; exit
		    (include <unistd.h>) ;; exit
		    (include <string.h>) ;; strcmp
		    (comment "I use integers that have the same size as a pointer")
		    (typedef uintptr_t uintgr)
		    (typedef intptr_t intgr)
		    ;; (gen-cmd (add 123 456))
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
		    (decl ((cons_object o1 (clist (cast 'o #x12) (cast 'o #x32)))
			   (cons_symbol o2 (clist *symbol* #x123))
			   (cons_number o3 (clist *number* #x324))))
		    (decl ((const char (aref builtin-name
					 (cl:length *builtin-function*)
					 buflen)
				  (builtin-function-name-clist))
			   (const uintgr (aref builtin-par-min
					   (cl:length *builtin-function*)))
			   (const uintgr (aref builtin-par-max
					   (cl:length *builtin-function*)))))
		    (decl ((o freelist)
			   (o tee)
			   (o global-env)
			   (o gc-stack)
			   (uintgr freespace)
			   (cons_object (aref workspace workspace-size))
			   (jmp_buf exception)
			   (char return-flag 0)
			   (char (aref buffer (+ buflen 1)))
			   (char last-char)))
		    
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
				   EVAL
				   EOF
				   last-char
				   ;;cmd
				   )
		    (comment "forward declarations")
		    (deftailrec-fw progn)
		    (function _eval ((o form)(o env)) -> o) (comment ";" :prefix "")
		    (function _read () -> o) (comment ";" :prefix "")
		    (function init-workspace () -> void
		      (dcomment "init-workspace")
		      (set freelist 0)
		      (for ((intgr i (- workspace-size 1))
			    (<= 0 i) (dec i))
			(decl ((o obj (+ workspace i)))
			  (set (%car obj) 0)
			  (set (%cdr obj) freelist)
			  (set freelist obj)
			  (inc freespace))))
		    (function erro ((const char* string)) -> void
		      (funcall printf "Error: %s\\n" string)
		      (set gc-stack 0)
		      ;; (funcall longjmp exception 1)
		      )
		    (function _alloc () -> o
		      (dcomment "alloc")
		      (when (== 0 freespace)
			  (err "No room"))
		      (decl ((o temp freelist))
			(set freelist (%cdr freelist))
			(set freespace (- freespace 1))
			(return temp)))
		    ;; (function myfree ((o obj)) -> void
		    ;;   (dcomment "free")
		    ;;   (set (%cdr obj) freelist)
		    ;;   (set freelist obj)
		    ;;   (inc freespace))
		    (function  _number ((intgr n)) -> o
		      (dcomment "number")
		      (decl ((cons_number* ptr
					   (cast 
					    'cons_number*
					    (funcall _alloc))))
			(set (cons-type ptr) *number*)
			(set (cons-integer ptr) n)
			(return (cast o ptr))))
		    (function _cons ((o arg1)
				     (o arg2)) -> o
		      (dcomment "cons")
		      (decl ((o ptr (cast 'o (funcall _alloc))))
			(set (%car ptr) arg1)
			(set (%cdr ptr) arg2)
			(return ptr)))
		    (function _symbol ((uintgr name)) -> o
		      (dcomment "symbol")
		      (decl ((cons_symbol* ptr
					   (cast 
					    'cons_symbol*
					    (funcall _alloc))))
			(set (cons-type ptr) *symbol*)
			(set (cons-name ptr) name)
			(return (cast o ptr))))
		    (function mark-object ((o obj)) -> void
		      (when (== 0 obj)
			  (return))
		      (when (marked obj)
			  (return))
		      (decl ((o arg (%car obj))
			     (intgr type (cons-type obj)))
			(mark obj)
			(when (and (!= *symbol* type)
				   (!= *number* type))
			    (funcall mark-object arg)
			    (funcall mark-object (%cdr obj)))))
		    (function sweep () -> void
		      (set freelist 0)
		      (set freespace 0)
		      (for ((int i (- workspace-size 1)) (<= 0 i) (dec i))
			(decl ((o obj (+ workspace i)))
			  (if (== 1 (marked obj))
			      (unmark obj)
			      (progn
				(set (%car obj) (cast 'o 0))
				(set (%cdr obj) freelist)
				(set freelist obj)
				(inc freespace))))))
		    (function gc ((o form) (o env)) -> void
		      (mark-object tee)
		      (mark-object global-env)
		      (mark-object gc-stack)
		      (mark-object form)
		      (mark-object env)
		      (funcall sweep))
		    
		    (function toradix40 ((intgr ch)) -> intgr
		      (when (== 0 ch)
			(return 0))
		      (when (and (<= #\0 ch) (<= ch #\9))
			(return (+ 30 (- ch #\0))))
		      (set ch (\| ch #x20))
		      (when (and (<= #\a ch) (<= ch #\z))
			(return (+ 1 (- ch #\a))))
		      (err "ill. char in sym")
		      (return 0))
		    (function fromradix40 ((intgr n)) -> intgr
		      (when (and (<= 1 n) (<= n 26))
			(return (+ n #\a -1)))
		      (when (and (<= 1 30) (<= n 39))
			(return (+ n #\0 -30)))
		      (when (== 27 n)
			(return #\-))
		      (return 0))
		    (function pack40 ((char* c)) -> uintgr
		      (return (+ (* 40 (+ (* 40 (toradix40 (aref c 0)))
					  (toradix40 (aref c 1))))
				 (toradix40 (aref c 2)))))
		    (function digitvalue ((char d)) -> intgr
		      (when (and (<= #\0 d)
				 (<= d #\9))
			(return (- d #\0)))
		      (set d (\| d #x20))
		      (when (and (<= #\a d)
				 (<= d #\f))
			(return (+ 10 (- d #\a ))))
		      (return 16))
		    (function lookupstring ((uintgr name)) -> char*
		      (for ((int i 0) (< i buflen) (inc i))
			(set (aref buffer i) (aref builtin-name name i)))
		      (return buffer))
		    (function name ((o obj)) -> char*
		      (set (aref buffer 3) (cast 'char 0))
		      (when (!= *symbol* (cons-type obj))
			(err "name"))
		      (decl ((uintgr x (cons-name obj)))
			(when (< x (builtin-function))
			  (return (funcall lookupstring x)))
			(for ((int n 2) (<= 0 n) (dec n))
			  (set (aref buffer n) (funcall fromradix40 (% x 40)))
			  (set x (/ x 40))))
		      (return buffer))
		    (function _integer ((o obj)) -> intgr
		      (when (!= *number* (cons-type obj))
			(err "not number"))
		      (return (cons-integer obj)))
		    (function issymbol ((o obj) (uintgr n)) -> int
		      (return (and (== *symbol* (cons-type obj))
				   (== n (cons-name obj)))))
		    (function _eq ((o a) (o b)) -> int
		      (return (or
				(== a b)
				(and (== *symbol* (cons-type a))
				     (== *symbol* (cons-type b))
				     (== (cons-name a) (cons-name b)))
				(and (== *number* (cons-type a))
				     (== *number* (cons-type b))
				     (== (cons-integer a) (cons-integer b))))))
		    (function value ((uintgr n) (o env)) -> o
		      (%dolist (item env)
			(when (== n (cons-name (%car item)))
			    (return item)))
		      (return cnil))
		    (function findvalue ((o var) (o env)) -> o
		      (decl ((uintgr varname (cons-name var))
			     (o pair (funcall value varname env)))
			(when (== NULL pair)
			  (set pair (funcall value varname global-env)))
			(when (== NULL pair)
			  (err "unknown var"))
			(return pair)))
		    (function findtwin ((o var) (o env)) -> o
		      (%dolist (item env)
			(when (== var (%car item))
			  (return item)))
		      (return cnil))
		    
		    (function closure ((int tail)
				       (o fname)
				       (o state)
				       (o function)
				       (o args)
				       (o* env)) -> o
		      (comment "(void) fname;" :prefix "")
		      (decl ((o params (%car function)))
			(set function (%cdr function))
			(comment "push state if not already in env")
			(%dolist (pair state)
			  (when (== NULL (funcall findtwin (%car pair) *env))
			    (_push pair *env)))
			(comment "add arguments to environment")
			(%dolist2 ((var params) (value args)) 
			  (if tail
			      (decl ((o pair (funcall findtwin var *env)))
				(if (!= NULL pair)
				    (set (%cdr pair) value)
				    (_push (funcall _cons var value) *env)))
			      (_push (funcall _cons var value) *env)))
			(when (!= NULL params)
			  (err "too few params"))
			(when (!= NULL args)
			  (err "too many params"))
			(comment "do implicit progn")
			(return (funcall tf-progn function *env))))
		    (function listlength ((o list)) -> int
		      (decl ((int len 0))
			(%dolist (e list)
			  (inc len))
			(return len)))
		    (function builtin ((char* n)) -> int
		      (decl ((intgr entry 0))
			(while (< entry (cl:length *builtin-function*))
			  (when (== 0 (funcall strcmp n (aref builtin-name entry)))
			    (return entry))
			  (inc entry))
			(return (cl:length *builtin-function*))))
		    (function lookupmin ((uintgr name)) -> int
		      (comment "(void) name;" :prefix "") ;; FIXME
		      (return (aref builtin-par-min name)))
		    (function lookupmax ((uintgr name)) -> int
		      (comment "(void) name;" :prefix "")
		      (return (aref builtin-par-max name)))
		    (decl ((fn_ptr_type
			    (aref builtin-fptr (cl:length *builtin-function*)))))
		    (function lookupfn ((uintgr name)) -> fn_ptr_type
		      (return (aref builtin-fptr name)))
		    (function _apply ((o function)
				      (o args)
				      (o* env)) -> o
		      (when (== *symbol* (cons-type function))
			  (decl ((uintgr name (cons-name function))
				 (int nargs (funcall listlength args)))
			    (when (<= (builtin-function) name)
			      (err "not a function"))
			    (when (< nargs (funcall lookupmin name))
			      (err "too few args"))
			    (when (< (funcall lookupmin name) nargs)
			      (err "too many args"))
			    (return (funcall
				     (cast 'fn_ptr_type
					   (funcall lookupfn name))
				     args *env))))
		      (when (and (_listp function)
				 (funcall issymbol (%car function)
					  (builtin-function-name-to-number
					   'lambda)))
			(set function (%cdr function))
			(decl ((o
				result
				(funcall closure 0
					 NULL NULL function args env)))
			  (return (funcall _eval result *env))))
		      (when (and (_listp function)
			       (funcall issymbol (%car function)
					(builtin-function-name-to-number
					 'closure)))
			  (set function (%cdr function))
			  (decl ((o
				  result
				  (funcall closure 0
					   NULL
					   (%car function)
					   (%cdr function)
					   args env)))
			    (return (funcall _eval result *env))))
		      (err "illegal function")
		      (return NULL))
		    (comment "checked car and cdr")
		    (function carx ((o arg)) -> o
		      (when (== 0 (_listp arg))
			(err "can't take car"))
		      (when (== cnil arg)
			(return cnil))
		      (return (%car arg)))
		    (function cdrx ((o arg)) -> o
		      (when (== 0 (_listp arg))
			(err "can't take cdr"))
		      (when (== cnil arg)
			(return cnil))
		      (return (%cdr arg)))
		    
		    (function _eval ((o form)
				     (o env)) -> o
		      (dcomment "eval")
		      (decl ((int TC 0))
			(comment "EVAL:" :prefix "") ;; FIXME this is crazy
			(when (< freespace 10)
			  (funcall gc form env))
			;; FIXME i left out some _end stuff and serial break
			(when (== NULL form)
			    (dcomment "nil")
			    (return cnil))
			(when (== *number* (cons-type form))
			  (dcomment "number")
			  (return form))
			(when (== *symbol* (cons-type form))
			  (decl ((uintgr name (cons-name form)))
			    (when (== (builtin-function-name-to-number 'nil)
				      name)
			      (dcomment "nil")
			      (return cnil))
			    (decl ((o pair (funcall value name env)))
			      (when (!= NULL pair)
				(progn (dcomment "sym cdr pair")
				       (return (%cdr pair))))
				(set pair (value name global-env))
				(if (!= NULL pair)
				    (progn (dcomment "sym cdr pair2")
					   (return (%cdr pair)))
				    (if (<= name (cl:length *builtin-function*))
					(progn (dcomment "form") (return form))
					(err "undefined variable"))))))
			(comment "it's a list")
			(decl ((o function (%car form))
			       (o args (%cdr form)))
			  (comment "list starting with symbol?")
			  (when (== *symbol* (cons-type function))
			      (decl ((uintgr name (cons-name function)))
				
				(when (== (builtin-function-name-to-number 'let)
					name)
				    ;; FIXME leaving out LETSTAR
				    
				    (decl ((o assigns (%car args))
					   (o forms (%cdr args))
					   (o newenv env))
				      (dcomment "process LET")
				      (%dolist (assign assigns)
					(if (_consp assign)
					    (_push (_cons (%car assign)
							  (funcall _eval
								   (_second assign)
								   env))
						   newenv)
					    (_push (_cons assign cnil)
						   newenv))
					;; FIXME letstar
					)
				      (set env newenv)
				      (set form (funcall tf_progn forms env))
				      (set TC 1)
				      (comment "goto EVAL;" :prefix "")))
				
				(when (== (builtin-function-name-to-number 'lambda) name)
				    (dcomment "process LAMBDA")
				    (when (== NULL env)
					(dcomment "lambda nil env")
					(return form))
				    (decl ((o envcopy NULL))
				      (%dolist (pair env) 
					(decl ((o val (%cdr pair)))
					  (when (== *number* (cons-type val))
					    (set val (funcall _number
							      (cons-integer val))))
					  (_push (funcall _cons (%car pair)
							  val)
						 envcopy)))
				      (progn (dcomment "call lambda")
					     (return (funcall
						      _cons
						      (funcall
						       _symbol
						       (builtin-function-name-to-number
							'closure))
						      (funcall _cons envcopy args))))))
				
				(when (and (< (builtin-function-name-to-number 'f_spec) name)
					   (< name (builtin-function-name-to-number 'f_tail)))
				    (dcomment "process SPECIAL form")
				    (return (funcall (cast 'fn_ptr_type (funcall lookupfn name))
						     args env)))
				(when (and (< (builtin-function-name-to-number 'f_tail) name)
					 (< name (builtin-function-name-to-number 'f_fun)))
				    (dcomment "process TAIL CALL form")
				    (set form (funcall (cast 'fn_ptr_type (funcall lookupfn name))
						       args env))
				    (set TC 1)
				    (comment "goto EVAL;" :prefix ""))))
			  (comment "evaluate the parameters - result in head")
			  (decl ((o fname (%car form))
				 (int TCstart TC)
				 (o head (funcall _cons
						  (funcall _eval
							   (%car form)
							   env)
						  NULL)))
			    (comment "don't gc the result list")
			    (_push head gc-stack) 
			    (decl ((o tail head))
			      (set form (%cdr form))
			      (decl ((int nargs 0))
				(%dolist (e form)
				  (decl ((o obj (funcall _cons
							 (funcall _eval e env)
							 NULL)))
				    (set (%cdr tail) obj)
				    (set tail obj)
				    (inc nargs)))
				(set function (%car head))
				(set args (%cdr head))
				(when (== *symbol* (cons-type function))
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
					(progn (dcomment "symbol")
					       (return result)))))
				(when (and (_listp function)
					 (funcall issymbol (%car function)
						  (builtin-function-name-to-number 'lambda)))
				    (set form (funcall closure TCstart
						       fname NULL (%cdr function) args
						       (addr-of env)))
				    (_pop gc-stack)
				    (set TC 1)
				    (dcomment "goto eval2")
				    (comment "goto EVAL;" :prefix ""))
				(when (and (_listp function)
					 (funcall issymbol (%car function)
						  (builtin-function-name-to-number 'closure)))
				    (set function (%cdr function))
				    (set form (funcall closure TCstart
						       fname (%car function) (%cdr function) args
						       (addr-of env)))
				    (_pop gc-stack)
				    (set TC 1)
				    (dcomment "goto eval3")
				    (comment "goto EVAL;" :prefix ""))
				(err "illegal func")
				(progn (dcomment "eval returns nil")
				       (return cnil))))))))
		    (function init-env () -> void
		      (set global-env NULL)
		      (set tee (funcall _symbol (builtin-function-name-to-number 'tee))))
		    (decl ((fn_ptr_type
			    (aref builtin-fptr (cl:length *builtin-function*))
			    (builtin-function-ptr-clist)
			    )))
		    
		    (function _getc () -> int
		      (when last-char
			(decl ((int temp last-char))
			  (set last-char 0)
			  (return temp)))
		      ;; (decl ((static int idx 0)
		      ;; 	     (int temp (aref cmd idx)))
		      ;; 	(inc idx)
		      ;; 	(funcall printf "%c" temp)
		      ;; 	(return temp))
		       (decl ((int temp (funcall getchar)))
		       	      (funcall printf "%c" temp)
		       	      (return temp))
		      )
		    (function nextitem () -> o
		      (dcomment "nextitem")
		      (decl ((int ch (funcall _getc)))
			(while (funcall isspace ch)
			  (set ch (funcall _getc)))
			(when (== #\; ch)
			    (while (!= #\( ch)
			      (set ch (funcall _getc)))
			    (set ch #\())
			(when (== #\newline ch)
			  (set ch (funcall _getc)))
			(when (== EOF ch)
			  (funcall exit 0))
			(when (== #\) ch)
			    (dcomment "ket")
			    (return (cast 'o *ket*)))
			(when (== #\( ch)
			    (dcomment "bra")
			    (return (cast 'o *bra*)))
			(when (== (char-code #\') ch)
			    (dcomment "quo")
			    (return (cast 'o *quo*)))
			(when (== #\. ch)
			    (dcomment "dot")
			    (return (cast 'o *dot*)))
			(comment "parse var or number")
			(decl ((intgr index 0)
			       (intgr base 10)
			       (intgr sign 1)
			       (uintgr result 0))
			  (if (== #\+ ch)
			      (progn
				(set (aref buffer index++) ch)
				(set ch (funcall _getc)))
			      (if (== #\- ch)
				  (progn (set sign -1)
					 (set (aref buffer index++) ch)
					 (set ch (funcall _getc)))
				  (if (== #\# ch)
				      (progn (set ch (\| (funcall _getc) #x20))
					     (if (== #\b ch)
						 (set base 2)
						 (if (== #\o ch)
						     (set base 8)
						     (if (== #\x ch)
							 (set base 16)
							 (err "illegal char after #"))))
					     (set ch (funcall _getc))))))
			  (decl ((intgr isnumber (< (funcall digitvalue ch) base)))
			    (comment "in case var is one letter")
			    (set (aref buffer 2) 0)
			    (while (and (== 0 (funcall isspace ch))
					(!= #\) ch)
					(< index buflen))
			      (set (aref buffer index++) ch)
			      (decl ((intgr temp (funcall digitvalue ch)))
				(set result (+ temp (* result base)))
				(set isnumber (and isnumber
						   (< (funcall digitvalue ch) base)))
				(set ch (funcall _getc))))
			    (set (aref buffer index) 0)
			    (when (== #\) ch)
			      (set last-char #\)))
			    (when isnumber
				(if (and (== base 10)
					 (< (+ (cast 'uintgr 32767)
					       (/ (- 1 sign)
						  2))
					    result))
				    (err "num out of range"))
				(dcomment "number")
				(return (funcall _number (* sign result))))
			    (decl ((intgr x (funcall builtin buffer)))
			      (when (== x (builtin-function-name-to-number 'nil))
				(dcomment "nil")
				(return cnil))
			      (if (< x (cl:length *builtin-function*))
				  (progn (dcomment "builtin symbol")
					 (return (funcall _symbol x)))
				  (progn (dcomment "usersymbol")
					 (return (funcall _symbol (funcall pack40 buffer))))))))))
		    (function read-rest () -> o
		      (decl ((o item (funcall nextitem)))
			(when (== (cast 'o *ket*) item)
			  (return NULL))
			(when (== (cast 'o *dot*) item)
			    (decl ((o arg1 (funcall _read)))
			      (if (!= NULL (funcall read-rest))
				  (err "malformed list"))
			      (return arg1)))
			(when (== (cast 'o *quo*) item)
			  (decl ((o arg1 (funcall _read)))
			      (return (funcall
				       _cons
				       (funcall
					_cons
					(funcall _symbol
						 (builtin-function-name-to-number 'quote))
					(funcall _cons arg1 NULL))
				       (funcall read-rest)))))
			(when (== (cast 'o *bra*) item)
			  (set item (funcall read-rest)))
			(return (funcall _cons item (funcall read-rest)))))
		    (function _print-object ((o form)) -> void
		      (dcomment "print-object")
		      (if (== NULL form)
			  (funcall printf "nil")
			  (if (and (_listp form)
				   (funcall issymbol (%car form)
					    (builtin-function-name-to-number 'closure)))
			      (funcall printf "<closure>")
			      (if (_listp form)
				  (progn
				    (funcall printf "(")
				    (funcall _print-object (%car form))
				    (set form (%cdr form))
				    (%dolist (e form) 
				      (if (_listp form)
					  (progn
					    (funcall printf " ")
					    (funcall _print-object e))))
				    (if (!= NULL form)
					(progn (funcall printf " . ")
					       (funcall _print-object form)))
				    (funcall printf ")"))
				  (if (== *number* (cons-type form))
				      (funcall printf "%ld" (funcall _integer form))
				      (if (== *symbol* (cons-type form))
					  (funcall printf "%s" (funcall name form))
					  (err "print err")))))))
		    (function _read () -> o
		      (dcomment "read")
		      (decl ((o item (funcall nextitem)))
			(when (== (cast 'o *bra*) item)
			  (return (funcall read-rest)))
			(when (== (cast 'o *dot*) item)
			  (return (funcall _read)))
			(when (== (cast 'o *quo*) item)
			  (return (funcall _cons
					   (funcall _symbol
						    (builtin-function-name-to-number 'quote))
					   (funcall _cons (funcall _read) NULL))))
			(return item)))
		    (function repl ((o env)) -> void
		      (dcomment "repl")
		      (for (() () ())
			(funcall gc NULL env)
			(funcall printf "freespace: %lu\\n" freespace)
			(funcall printf "> ")
			(decl ((o line (funcall _read)))
			  (when (== cnil line)
			      (funcall printf "\\n")
			      (return))
			  (funcall printf "\\n")
			  (_push line gc-stack)
			  ;;(funcall printf "push line gc-stack\\n")
			  (funcall _print-object (funcall _eval line env))
			  (_pop gc-stack)
			  (funcall printf "\\n\\n"))))
		    (decl ((const uintgr (aref builtin-par-min
					   (cl:length *builtin-function*))
				  (builtin-function-min-clist))
			   (const uintgr (aref builtin-par-max
					   (cl:length *builtin-function*))
				  (builtin-function-max-clist))))
		    (function main ((int argc) (char** argv)) -> int
		      (funcall init-workspace)
		      (funcall init-env)
		      (repl NULL)
		      ;; (decl ((o line (funcall _read)))
		      ;;  (funcall _print-object (funcall _eval line env)))
		      (return 0))) 
       do
	 (simple-print e))))


