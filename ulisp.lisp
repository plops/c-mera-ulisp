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

(defparameter *boiler-func* nil)
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
  `(def-with-prefix (fn ,name ,min ,max) ,@body))

(defmacro %function (name parameters -> type &body body)
  `(cl:push '((:name ,name)
	      (:fwd  (function ,name ,parameters -> ,type))
	      (:code (function ,name ,parameters -> ,type ,@body))) *boiler-func*))

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
(calc-builtin-name-list *builtin-special*)

(defmacro gen-builtin-strings ()
"Create strings like this:
const char builtin_special_name[9][6] = { 'decf', 'incf', 'pop', 'push', 'loop', 'setq', 'defvar', 'defun', 'quote' };
const char builtin_tailrec_name[6][6] = { 'or', 'and', 'cond', 'if', 'return', 'progn' };
const char builtin_normalfunc_name[9][5] = { 'add', 'apply', 'cdr', 'car', 'eq', 'listp', 'atom', 'cons', 'not' };
"
  `(cl:progn
     (use-variables
       builtin-special-name
       builtin-tailrec-name
       builtin-normalfunc-name)
     (decl ((const char (aref builtin-special-name
			  ,(cl:length *builtin-special*)
			  ,(calc-builtin-name-max-len *builtin-special*)
			  )
		  (clist ,@(calc-builtin-name-list *builtin-special*)))
	   (const char (aref builtin-tailrec-name
			 ,(cl:length *builtin-tailrec*)
			 ,(calc-builtin-name-max-len *builtin-tailrec*)
			 )
		  (clist ,@(calc-builtin-name-list *builtin-tailrec*)))
	   (const char (aref builtin-normalfunc-name
			 ,(cl:length *builtin-normalfunc*)
			 ,(calc-builtin-name-max-len *builtin-normalfunc*))
		  (clist ,@(calc-builtin-name-list *builtin-normalfunc*))))
      )))

#+nil
(gen-builtin-strings)

(defun calc-builtin-fptr-list (prefix l)
  "PREFIX is sp_, tf_ or fn_. L is the global list with the functions,
e.g. *builtin-special*. Example output: (sp_decf sp_incf sp_pop
sp_push sp_loop sp_setq sp_defvar sp_defun sp_quote)"
  (mapcar #'(lambda (x) (intern (string-upcase (format nil "~a~a" prefix x))))
	  (mapcar #'get-builtin-name l)))

(defmacro gen-builtin-fptr-clists ()
  "Create arrays of function pointers, like this:
fn_ptr_type builtin_special_fptr[9] = { sp_decf, sp_incf, sp_pop,
sp_push, sp_loop, sp_setq, sp_defvar, sp_defun, sp_quote };
fn_ptr_type builtin_tailrec_fptr[6] = { tf_or, tf_and, tf_cond, tf_if,
tf_return, tf_progn };
fn_ptr_type builtin_normalfunc_fptr[9] = { fn_add, fn_apply, fn_cdr,
fn_car, fn_eq, fn_listp, fn_atom, fn_cons, fn_not };
"
  `(cl:progn
     (use-variables
       builtin-special-fptr
       builtin-tailrec-fptr
       builtin-normalfunc-fptr)
     (decl ((fn_ptr_type (aref builtin-special-fptr ,(cl:length *builtin-special*))
		  (clist ,@(calc-builtin-fptr-list "sp_" *builtin-special*)))
	   (fn_ptr_type (aref builtin-tailrec-fptr ,(cl:length *builtin-tailrec*))
		  (clist ,@(calc-builtin-fptr-list "tf_" *builtin-tailrec*)))
	   (fn_ptr_type (aref builtin-normalfunc-fptr ,(cl:length *builtin-normalfunc*))
		  (clist ,@(calc-builtin-fptr-list "fn_" *builtin-normalfunc*)))))))

;; const uintgr builtin_par_max[33] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

;; (const uintgr (aref builtin-par-min
;; 		(cl:length *builtin-function*))
;;        (builtin-function-min-clist))

(defmacro builtin-function-min-clist ()
  `(clist ,@(mapcar #'(lambda (x) (cl:let ((y (builtin-function-min x)))
				    (cl:if y y 0)))
		    *builtin-function*)))

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
(eval-when (:compile-toplevel)
  (setf *builtin-special* nil
	*builtin-tailrec* nil
	*builtin-normalfunc* nil)
  (setf *boiler-func* nil))

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
  (load "boiler"))

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
		    (gen-builtin-strings)
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
		    (comment "#undef NULL" :prefix "")
		    (decl ((o freelist)
			   (o tee)
			   (o global-env)
			   (o gc-stack)
			   (uintgr freespace)
			   (cons_object (aref workspace workspace-size))
			   (jmp_buf exception)
			   (char return-flag 0)
			   (char (aref buffer (+ buflen 1)))
			   (char last-char)
			   (void* NULL 0)))
		    

		    (comment "forward declarations")
		    (gen-builtin-forward-declaration)
		    (gen-builtin-fptr-clists)
		    (gen-builtin-code)
		    
		    (decl ((const uintgr (aref builtin-par-min
					   (cl:length *builtin-function*))
				  (builtin-function-min-clist))
			   (const uintgr (aref builtin-par-max
					   (cl:length *builtin-function*))
				  (builtin-function-max-clist))))
		    (decl ((fn_ptr_type
			    (aref builtin-fptr (cl:length *builtin-function*))
			    (builtin-function-ptr-clist))))
		    (function main ((int argc) (char** argv)) -> int
		      (funcall init-workspace)
		      (funcall init-env)
		      (repl NULL)
		      ;; (decl ((o line (funcall _read)))
		      ;;  (funcall _print-object (funcall _eval line env)))
		      (return 0))) 
       do
	 (simple-print e))))


