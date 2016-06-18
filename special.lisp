(in-package :cg-user)
(defspecial (quote 1 1)
  (comment "(void) env;" :prefix "")
  (return (%car args)))
(defspecial (defun 0 127) 
  (comment "(void) env;" :prefix "")
  (decl ((o var (%car args)))
    (ensure-symbol var)
    (decl ((o
	    val
	    (funcall
	     _cons
	     (funcall
	      _symbol
	      (builtin-function-name-to-number 'lambda))
	     (%cdr args)))
	   (o pair
	      (funcall value (cons-name var)
		       global-env)))
      (when (!= NULL pair)
	(set (%cdr pair) val)
	(return var))
      (_push (funcall _cons var val) global-env)
      (return var))))
(defspecial (defvar 0 127)
  (decl ((o var (%car args)))
    (ensure-symbol var)
    (decl ((o val (funcall _eval (%second args)
			   env))
	   (o pair (funcall value (cons-name var)
			    global-env)))
      (when (!= NULL pair)
	(set (%cdr pair) val)
	(return var))
      (%push (funcall _cons var val)
	     global-env)
      (return var))))
(defspecial (setq 2 2)
  (decl ((o arg (funcall _eval
			 (%second args)
			 env))
	 (o pair (funcall findvalue
			  (%car args)
			  env)))
    (set (%cdr pair) arg)
    (return arg)))
(defspecial (loop 0 127)
  (set return-flag 0)
  (decl ((o start args))
    (for (() () ())
      (set args start)
      (%dolist (form args)
	(decl ((o result (funcall _eval form env)))
	  (when (== 1 return-flag)
	    (set return-flag 0)
	    (return result)))))))
(defspecial (push 2)
  (decl ((o item (funcall _eval (%car args) env))
	 (o pair (funcall findvalue (%second args)
			  env)))
    (%push item (%cdr pair))
    (return (%cdr pair))))
(defspecial (pop 1)
  (decl ((o pair (funcall findvalue (%second args)
			  env))
	 (o result (%car (%cdr pair))))
    (%pop (%cdr pair))
    (return result)))
(defspecial (incf 1 2)
  (decl ((o var (%car args))
	 (o pair (funcall findvalue var env))
	 (int result (funcall _integer (funcall _eval var env)))
	 (int temp 1))
    (when (!= NULL (%cdr args))
      (set temp (funcall _integer (funcall _eval (%second args) env))))
    (set result (+ result temp))
    (set var (funcall _number result))
    (set (%cdr pair) var)
    (return var)))
(defspecial (decf 1 2)
  (decl ((o var (%car args))
	 (o pair (funcall findvalue var env))
	 (int result (funcall _integer (funcall _eval var env)))
	 (int temp 1))
    (when (!= NULL (%cdr args))
      (set temp (funcall _integer
			 (funcall _eval (%second args) env))))
    (set result (- result temp))
    (set var (funcall _number result))
    (set (%cdr pair) var)
    (return var)))
