(in-package :cg-user)
(deftailrec (progn 0 127)
  (when (== NULL args)
    (return cnil))
  (decl ((o more (%cdr args)))
    (%dolist (e more)
      (funcall _eval (%car args) env)
      (set args more))
    
    (return (%car args))))
(deftailrec (return 0 127)
  (set return-flag 1)
  (return (funcall tf_progn args env)))
(deftailrec (if 2 3)
  (when (!= cnil (funcall _eval (%car args) env))
    (return (_second args)))
  (return (%third args)))
(deftailrec (cond 0 127)
  (%dolist (clause args)
    (decl ((o test (funcall _eval (%car clause) env))
	   (o forms (%cdr clause)))
      (when (!= cnil test)
	(if (== NULL forms)
	    (return test)
	    (return (funcall tf_progn forms env))))))
  (return cnil))
(deftailrec (and 0 127)
  (when (== NULL args)
    (return tee))
  (decl ((o more (%cdr args)))
    (%dolist (e more)
      (when (== NULL (funcall _eval (%car args) env))
	(return cnil))
      (set args more))
    (return (%car args))))
(deftailrec (or 0 127)
  (decl ((o more (%cdr args)))
    (%dolist (e more)
      (decl ((o result (funcall _eval (%car args) env)))
	(when (!= NULL result)
	  (return result)))
      (set args more))
    (return (%car args))))
