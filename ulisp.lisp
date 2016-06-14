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


(defparameter *builtin-functions*
  '((symbols)
    (nil)
    (tee)
    (lambda)
    (let)
    (letstar)
    (closure)
    (special_forms)
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
    (listp)))

(defmacro gen-builtin-table ()
  `(decl ,(loop for (e) in *builtin-functions* and i from 0 collect
		`(const char
			,(cl:intern (cl:format nil "STRING~3,'0d" i))
			,(cl:format nil "~a" e)))))

#+nil
(let ((workspace-size 315)
      (buflen 17) ;; length of longest symbol 
      )
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
		   (decl ((const char string000 "bla")))
		   #+nil (gen-builtin-table)
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
				  UINTPTR_MAX)
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
		   (function myalloc () -> cons_object*
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
		   (function  make-number ((intgr n)) -> cons_object*
		     (decl ((cons_number* ptr
					  (cast 
					   'cons_number*
					   (funcall myalloc))))
		       (set (pref ptr type) *number*)
		       (set (pref ptr integer) n)
		       (return (cast cons_object* ptr))))
		   (function make-cons ((cons_object* arg1)
					(cons_object* arg2)) -> cons_object*
		     (decl ((cons_object* ptr (cast 'cons_object*
						    (funcall myalloc))))
		       (set (pref ptr car) arg1)
		       (set (pref ptr cdr) arg2)
		       (return ptr)))
		   (function make-csymbol ((uintgr name)) -> cons_object*
		       (decl ((cons_symbol* ptr
					  (cast 
					   'cons_symbol*
					   (funcall myalloc))))
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
		   (function pack40 ((char* c)) -> intgr
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
		     (return buffer))
		   (function name ((cons_object* obj)) -> char*
		     (set (aref buffer 3) (cast 'char 0))
		     (if (!= *symbol* (cons-type obj))
			 (erro "name"))
		     (decl ((uintgr x (cons-name obj)))
		       (if (< x (cl:length *builtin-functions*))
			   (return (funcall lookupstring x)))
		       (for ((int n 2) (<= 0 n) --n)
			 (set (aref buffer n) (funcall fromradix40 (% x 40)))
			 (set x (/ x 40)))))
		   (function main () -> int
		     (funcall printf "%lx\\n" *mark-bit*)
		     (return 0))) 
      do
	(simple-print e))))


