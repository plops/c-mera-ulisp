(in-package :cg-user)
(switch-reader)
(deffunction (not 1)
  (comment "(void) env;" :prefix "")
  (if (== cnil (%car args))
      (return tee)
      (return cnil)))
(deffunction (cons 2)
  (comment "(void) env;" :prefix "")
  (return (funcall _cons (%car args)
		   (%second args))))
(deffunction (atom 1)
  (comment "(void) env;" :prefix "")
  (decl ((o arg1 (%car args)))
    (if (%consp arg1)
	(return cnil)
	(return tee))))
(deffunction (listp 1)
  (comment "(void) env;" :prefix "")
  (decl ((o arg1 (%car args)))
    (if (%listp arg1)
	(return tee)
	(return cnil))))
(deffunction (eq 2)
  (comment "(void) env;" :prefix "")
  (decl ((o arg1 (%car args))
	 (o arg2 (%second args)))
    (if (funcall _eq arg1 arg2)
	(return tee)
	(return cnil))))
(deffunction (car 1)
  (comment "(void) env;" :prefix "")
  (return (funcall carx (%car args))))
(deffunction (cdr 1)
  (comment "(void) env;" :prefix "")
  (return (funcall cdrx (%car args))))
(deffunction (apply 2 127)
  (decl ((o previous NULL)
	 (o last args))
    (%dolist (e (%cdr last))
      (set previous last))
    (when (== 0 (%listp (%car last)))
      (%err "last arg not list"))
    (set (%cdr previous) (%car last))
    (return (funcall _apply (%car args)
		     (%cdr args)
		     (addr-of env)))))
(deffunction (add 0 127)
  (comment "(void) env;" :prefix "")
  (decl ((intgr result 0))
    (%dolist (item args)
      (decl ((intgr temp (funcall _integer item)))
	(set result (+ result temp))))
    (return (funcall _number result))))

(deffunction (less 1 127)
    (comment "(void) env;" :prefix "")
  (decl ((intgr arg1 (funcall _integer (%car args))))
	(set args (%cdr args))
	(%dolist (item args)
		 (decl ((intgr arg2 (funcall _integer item)))
		       (when (== 0 (< arg1 arg2))
			 (return cnil))
		       (set arg1 arg2)))
    (return tee)))
