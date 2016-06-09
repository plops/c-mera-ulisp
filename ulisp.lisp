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



(let ((workspace-size 315)
      (buflen 13))
 (with-open-file (*standard-output* "ulisp.ino"
				    :direction :output
				    :if-exists :supersede
				    :if-does-not-exist :create)
   (loop for e in (list
		   (include <setjmp.h>)
		   (struct bla
		     (decl ((int n))))
		   (function init-workspace () -> void
		       (let ((freelist 0))
			 (dotimes (i workspace-size)
			  ))))
      do
	(simple-print e))))
