(in-package :cg-user)
(switch-reader)
(%function init-workspace () -> void
  (dcomment "Construct the freelist and set freespace to the initial value.")
  (set freelist 0)
  (for ((intgr i (- workspace-size 1))
	(<= 0 i) (dec i))
    (decl ((o obj (+ workspace i)))
      (set (%car obj) 0)
      (set (%cdr obj) freelist)
      (set freelist obj)
      (inc freespace))))
(%function erro ((const char* string)) -> void
  (funcall printf "Error: %s\\n" string)
  (set gc-stack 0)
  ;; (funcall longjmp exception 1)
  )
(%function _alloc () -> o
  (dcomment "Return an element from the freelist and reduce freespace.")
  (when (== 0 freespace)
    (err "No room"))
  (decl ((o temp freelist))
    (set freelist (%cdr freelist))
    (set freespace (- freespace 1))
    (return temp)))
;; (%function myfree ((o obj)) -> void
;;   (dcomment "free")
;;   (set (%cdr obj) freelist)
;;   (set freelist obj)
;;   (inc freespace))
(%function  _number ((intgr n)) -> o
  (dcomment "Allocate an object and store the integer inside.")
  (decl ((cons_number* ptr
		       (cast 
			'cons_number*
			(funcall _alloc))))
    (set (cons-type ptr) *number*)
    (set (cons-integer ptr) n)
    (return (cast o ptr))))
(%function _cons ((o arg1)
		  (o arg2)) -> o
  (dcomment "Allocate an object and attach both arguments.")
  (decl ((o ptr (cast 'o (funcall _alloc))))
    (set (%car ptr) arg1)
    (set (%cdr ptr) arg2)
    (return ptr)))
(%function _symbol ((uintgr name)) -> o
  (dcomment "Allocate an object and store the symbol (with up to 3 characters) inside.")
  (decl ((cons_symbol* ptr
		       (cast 
			'cons_symbol*
			(funcall _alloc))))
    (set (cons-type ptr) *symbol*)
    (set (cons-name ptr) name)
    (return (cast o ptr))))
(%function mark-object ((o obj)) -> void
  (dcomment "Set the most significant bit of the pointer to 1 (mark). Iterate through car and cdr of obj.")
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
(%function sweep () -> void
  (dcomment "Clear freespace, go through the workspace and attach all unmarked objects to freespace.")
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
(%function gc ((o form) (o env)) -> void
  (dcomment "Mark all objects in the lists tee, global-env, gc-stack, the parameters form and env and reclaim space from objects that haven't been marked.")
  (mark-object tee)
  (mark-object global-env)
  (mark-object gc-stack)
  (mark-object form)
  (mark-object env)
  (funcall sweep))

;; (cl:+ 11 26)

(%function toradix40 ((intgr ch)) -> intgr
;; 40 character alphabet
;; ascii code 0   48  49  50  51  52  53  54  55  56  57  
;; ascii char NUL  0   1   2   3   4   5   6   7   8   9   
;; radix-40   0   30  31  32  33  34  35  36  37  38  39  

;; 65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  
;; A   B   C   D   E   F   G   H   I   J   K   L   M   N   O   P   Q   R   S   T   U   V   W   X   Y   Z   
;; 1   2   3   4   5   6   7   8   9   10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  

;; 97  98  99  100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 
;; a   b   c   d   e   f   g   h   i   j   k   l   m   n   o   p   q   r   s   t   u   v   w   x   y   z   
;; 1   2   3   4   5   6   7   8   9   10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  

  (dcomment "Convert ascii character into an alphabet with 40 elements (Its actually 37).  Alphabet contains Null-character, 0-9, and a-z. Not case sensitive.")
  (when (== 0 ch)
    (return 0))
  (when (and (<= #\0 ch) (<= ch #\9))
    (return (+ 30 (- ch #\0))))
  (set ch (\| ch #x20))
  (when (and (<= #\a ch) (<= ch #\z))
    (return (+ 1 (- ch #\a))))
  (err "ill. char in sym")
  (return 0))
(%function fromradix40 ((intgr n)) -> intgr
  (dcomment "Convert a radix 40 character into an ascii character.")
  (when (and (<= 1 n) (<= n 26))
    (return (+ n #\a -1)))
  (when (and (<= 1 30) (<= n 39))
    (return (+ n #\0 -30)))
  (when (== 27 n)
    (return #\-))
  (return 0))
(%function pack40 ((char* c)) -> uintgr
  (dcomment "Combine three ascii characters into a radix-40 that is represented as a 16-bit value.")
  (return (+ (* 40 (+ (* 40 (toradix40 (aref c 0)))
		      (toradix40 (aref c 1))))
	     (toradix40 (aref c 2)))))
(%function digitvalue ((char d)) -> intgr
;; 47: (16 #\/)
;; 48: (0 #\0)
;; 49: (1 #\1)
;; 50: (2 #\2)
;; 51: (3 #\3)
;; 52: (4 #\4)
;; 53: (5 #\5)
;; 54: (6 #\6)
;; 55: (7 #\7)
;; 56: (8 #\8)
;; 57: (9 #\9)
;; 58: (16 #\:)
;; ...
;; 96: (16 #\`)
;; 97: (10 #\a)
;; 98: (11 #\b)
;; 99: (12 #\c)
;; 100: (13 #\d)
;; 101: (14 #\e)
;; 102: (15 #\f)
;; 103: (16 #\g)
  (dcomment "Convert the ascii characters 0-9 into the digitvalues. The characters a-f are converted into 10..15 respectively. Any othercharacter returns 16.")
  (when (and (<= #\0 d)
	     (<= d #\9))
    (return (- d #\0)))
  (set d (\| d #x20))
  (when (and (<= #\a d)
	     (<= d #\f))
    (return (+ 10 (- d #\a ))))
  (return 16))
(%function lookupstring ((uintgr idx)) -> char*
  (dcomment "Given an index number copy the string for the corresponding built-in function name into the global character array buffer.")
  (for ((int i 0) (< i buflen) (inc i))
    (set (aref buffer i) (aref builtin-name idx i)))
  (return buffer))
(%function name ((o obj)) -> char*
  (dcomment "If an object is of type symbol, write its name into the global character array buffer. Otherwise throw error.")
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
(%function _integer ((o obj)) -> intgr
  (dcomment "If an object is of type number, return the value as an integer. Otherwise throw error.")
  (when (!= *number* (cons-type obj))
    (err "not number"))
  (return (cons-integer obj)))
(%function issymbol ((o obj) (uintgr n)) -> int
  (dcomment "If an object OBJ is a of type symbol return true if the builtin function index/symbol corresponds to the same function/symbol.")
  (return (and (== *symbol* (cons-type obj))
	       (== n (cons-name obj)))))
(%function _eq ((o a) (o b)) -> int
  (dcomment "Compare two objects. They are considered the same if both objects are the same pointer, if both are symbols with identical names or if both are numbers with identical values.")
  (return (or
	    (== a b)
	    (and (== *symbol* (cons-type a))
		 (== *symbol* (cons-type b))
		 (== (cons-name a) (cons-name b)))
	    (and (== *number* (cons-type a))
		 (== *number* (cons-type b))
		 (== (cons-integer a) (cons-integer b))))))
(%function value ((uintgr n) (o env)) -> o
  (dcomment "Lookup a user-defined symbol (stored as at most 3 characters in radix-40 encoding in N) in the environment ENV.")
  (%dolist (item env)
    (when (== n (cons-name (%car item)))
      (return item)))
  (return cnil))
(%function findvalue ((o var) (o env)) -> o
  (dcomment "Return the place that stores the symbol VAR. The lookup is first in ENV then in GLOBAL-ENV.")
  (decl ((uintgr varname (cons-name var))
	 (o pair (funcall value varname env)))
    (when (== NULL pair)
      (set pair (funcall value varname global-env)))
    (when (== NULL pair)
      (err "unknown var"))
    (return pair)))
(%function findtwin ((o var) (o env)) -> o
  (%dolist (item env)
    (when (== var (%car item))
      (return item)))
  (return cnil))

(%function closure ((int tail)
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
	(%push pair *env)))
    (comment "add arguments to environment")
    (%dolist2 ((var params) (value args)) 
      (if tail
	  (decl ((o pair (funcall findtwin var *env)))
	    (if (!= NULL pair)
		(set (%cdr pair) value)
		(%push (funcall _cons var value) *env)))
	  (%push (funcall _cons var value) *env)))
    (when (!= NULL params)
      (err "too few params"))
    (when (!= NULL args)
      (err "too many params"))
    (comment "do implicit progn")
    (return (funcall tf-progn function *env))))
(%function listlength ((o list)) -> int
  (decl ((int len 0))
    (%dolist (e list)
      (inc len))
    (return len)))
(%function builtin ((char* name)) -> int
  (decl ((intgr entry 0))
    (while (< entry (cl:length *builtin-function*))
      (when (== 0 (funcall strcmp name (aref builtin-name entry)))
	(return entry))
      (inc entry))
    (return (cl:length *builtin-function*))))
(%function lookupmin ((uintgr name)) -> int
  (comment "(void) name;" :prefix "") ;; FIXME
  (return (aref builtin-par-min name)))
(%function lookupmax ((uintgr name)) -> int
  (comment "(void) name;" :prefix "")
  (return (aref builtin-par-max name)))
(decl ((fn_ptr_type
	(aref builtin-fptr (cl:length *builtin-function*)))))
(%function lookupfn ((uintgr name)) -> fn_ptr_type
  (return (aref builtin-fptr name)))
(%function _apply ((o function)
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
  (when (and (%listp function)
	     (funcall issymbol (%car function)
		      (builtin-function-name-to-number
		       'lambda)))
    (set function (%cdr function))
    (decl ((o
	    result
	    (funcall closure 0
		     NULL NULL function args env)))
      (return (funcall _eval result *env))))
  (when (and (%listp function)
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
;; (comment "checked car and cdr")
(%function carx ((o arg)) -> o
  (when (== 0 (%listp arg))
    (err "can't take car"))
  (when (== cnil arg)
    (return cnil))
  (return (%car arg)))
(%function cdrx ((o arg)) -> o
  (when (== 0 (%listp arg))
    (err "can't take cdr"))
  (when (== cnil arg)
    (return cnil))
  (return (%cdr arg)))

(%function _eval ((o form)
		 (o env)) -> o
  (dcomment "eval")
  (decl ((int TC 0))
    (comment "EVALJUMP:" :prefix "") ;; FIXME this is crazy
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
		(if (%consp assign)
		    (%push (funcall _cons (%car assign)
				    (funcall _eval
					     (%second assign)
					     env))
			   newenv)
		    (%push (funcall _cons assign cnil)
			   newenv))
		;; FIXME letstar
		)
	      (set env newenv)
	      (set form (funcall tf_progn forms env))
	      (set TC 1)
	      (comment "goto EVALJUMP;" :prefix "")))
	  
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
		  (%push (funcall _cons (%car pair)
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
	    (comment "goto EVALJUMP;" :prefix ""))))
      (comment "evaluate the parameters - result in head")
      (decl ((o fname (%car form))
	     (int TCstart TC)
	     (o head (funcall _cons
			      (funcall _eval
				       (%car form)
				       env)
			      NULL)))
	(comment "don't gc the result list")
	(%push head gc-stack) 
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
		  (%pop gc-stack)
		  (progn (dcomment "symbol")
			 (return result)))))
	    (when (and (%listp function)
		       (funcall issymbol (%car function)
				(builtin-function-name-to-number 'lambda)))
	      (set form (funcall closure TCstart
				 fname NULL (%cdr function) args
				 (addr-of env)))
	      (%pop gc-stack)
	      (set TC 1)
	      (dcomment "goto eval2")
	      (comment "goto EVALJUMP;" :prefix ""))
	    (when (and (%listp function)
		       (funcall issymbol (%car function)
				(builtin-function-name-to-number 'closure)))
	      (set function (%cdr function))
	      (set form (funcall closure TCstart
				 fname (%car function) (%cdr function) args
				 (addr-of env)))
	      (%pop gc-stack)
	      (set TC 1)
	      (dcomment "goto eval3")
	      (comment "goto EVALJUMP;" :prefix ""))
	    (err "illegal func")
	    (progn (dcomment "eval returns nil")
		   (return cnil))))))))
(%function init-env () -> void
  (set global-env NULL)
  (set tee (funcall _symbol (builtin-function-name-to-number 'tee))))

(%function _getc () -> int
  (when last-char
    (decl ((int temp last-char))
      (set last-char 0)
      (return temp)))
  ;; (decl ((static int idx 0)
  ;; 	 (int temp (aref cmd idx)))
  ;; 	(inc idx)
  ;; 	(funcall printf "%c" temp)
  ;; 	(return temp))
  (decl ((int temp (funcall getchar)))
    (funcall printf "%c" temp)
    (return temp))
  )
(%function nextitem () -> o
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
(%function read-rest () -> o
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
(%function _print-object ((o form)) -> void
  (dcomment "print-object")
  (if (== NULL form)
      (funcall printf "nil")
      (if (and (%listp form)
	       (funcall issymbol (%car form)
			(builtin-function-name-to-number 'closure)))
	  (funcall printf "<closure>")
	  (if (%listp form)
	      (progn
		(funcall printf "(")
		(funcall _print-object (%car form))
		(set form (%cdr form))
		(%dolist (e form) 
		  (if (%listp form)
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
(%function _read () -> o
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
(%function repl ((o env)) -> void
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
      (%push line gc-stack)
      ;;(funcall printf "push line gc-stack\\n")
      (funcall _print-object (funcall _eval line env))
      (%pop gc-stack)
      (funcall printf "\\n\\n"))))
