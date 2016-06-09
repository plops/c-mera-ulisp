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

(defmacro cons-car (x)
  `(pref (cast '(struct cons_object*) ,x) car))
(defmacro cons-cdr (x)
  `(pref (cast '(struct cons_object*) ,x) cdr))

(defmacro deftstruct (name &body body)
  `(progn
     (struct ,name
       ,@body)
     (typedef struct ,name ,name)))

(defmacro err (&rest rest))

(let ((workspace-size 315)
      (buflen 13))
 (with-open-file (*standard-output* "ulisp.ino"
				    :direction :output
				    :if-exists :supersede
				    :if-does-not-exist :create)
   (loop for e in (list
		   (include <setjmp.h>)
		   (deftstruct cons_object
		     (decl ((cons_object* car)
			    (cons_object* cdr))))
		   (deftstruct cons_symbol
		     (decl ((unsigned int type)
			    (unsigned int name))))
		   (deftstruct cons_number
		     (decl ((unsigned int type)
			    (int integer))))
		   (decl ((cons_object* freelist)
			  (unsigned int freespace)
			  (cons_object (aref workspace workspace-size))))
		   (use-variables freelist
				  freespace
				  workspace)
		   (function init-workspace () -> void
		     (set freelist 0)
		     (for ((int i 0) (< i workspace-size) ++i)
		       (decl ((struct cons_object* obj (+ workspace i)))
			 (set (cons-car obj) 0)
			 (set (cons-cdr obj) freelist)
			 (set freelist obj)
			 freespace++)))
		   (function myalloc () -> cons_object*
		     (if (== 0 freespace)
			 (err "No room"))
		     (decl ((cons_object* temp freelist))
		       (set freelist (cons-cdr freelist))
		       freespace--
		       (return temp))))
      do
	(simple-print e))))
