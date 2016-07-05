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
;; (%function erro ((const char* string)) -> void
;;   (%puts "Error: ")
;;   (funcall _putsn string)
;;   (funcall _putchar #\Newline)
;;   (set gc-stack 0)
;;   ;; (funcall longjmp exception 1)
;;   )
(%function _alloc () -> o
  (dcomment "Return an element from the freelist and reduce freespace.")
  (when (== 0 freespace)
    (%err "No room"))
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
  (%err "ill. char in sym")
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
  (for ((int i 0) (< i (calc-builtin-name-max-len *builtin-declaration*)) (inc i))
    (set (aref buffer i) (aref builtin-name idx i)))
  (return buffer))
(%function name ((o obj)) -> char*
  (dcomment "If an object is of type symbol, write its name into the global character array buffer. Otherwise throw error.")
  (set (aref buffer 3) (cast 'char 0))
  (when (!= *symbol* (cons-type obj))
    (%err "name"))
  (decl ((uintgr x (cons-name obj)))
    (when (< x (cl:length *builtin-declaration*))
      (return (funcall lookupstring x)))
    (for ((int n 2) (<= 0 n) (dec n))
      (set (aref buffer n) (funcall fromradix40 (% x 40)))
      (set x (/ x 40))))
  (return buffer))
(%function _integer ((o obj)) -> intgr
  (dcomment "If an object is of type number, return the value as an integer. Otherwise throw error.")
  (when (!= *number* (cons-type obj))
    (%err "not number"))
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
      (%err "unknown var"))
    (return pair)))
(%function findtwin ((o var) (o env)) -> o
  (dcomment "Find VAR in environment ENV and return it, otherwise NIL.")
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
  ;; (defun mt (x)
  ;;   (lambda ()
  ;;     (digitalwrite x (not (digitalread x)))
  ;; example for a closure:
  ;; (defvar grn (mt 6))

  ;; (closure :tail 0 :fname n :state ? :function ((a b) (+ a b x)) :args (1 2) :env (:x 3))
  
  (dcomment "FIXME I guess this is one of the most complicated concepts. As such I probably have to revisit this after understanding apply")
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
      (funcall _print-object function)
      (%err "too few params"))
    (when (!= NULL args)
      (%err "too many params"))
    (comment "do implicit progn")
    (return (funcall tf_progn function *env))))
(%function listlength ((o list)) -> int
  (dcomment "Return the length of a list.")
  (decl ((int len 0))
    (%dolist (e list)
      (inc len))
    (return len)))
(%function _string_eq_p ((const char* a) (const char* b) ;; (int n)
			 ) -> int
  (decl ((const int n (calc-builtin-name-max-len *builtin-declaration*)))
   (for ((int i 0)
	 (and (< i n)
	      (aref a i)
	      (aref b i))
	 (inc i))
     (if (!= (aref a i) (aref b i))
	 (if (< (cast 'uchar (aref a i))
		(cast 'uchar (aref b i)))
	     (return -1)
	     (return 1))
	 (when (== 0 (aref a i))
	   (return 0)))))
  (return 0))
(%function builtin ((char* name)) -> int
  (dcomment "Find the index of a builtin function with the name given as a string.")
  (decl ((intgr entry 0))
    (while (< entry (cl:length *builtin-declaration*))
      ;; (when (== 0 (funcall _string_eq_p
      ;; 			   (aref builtin-name entry)
      ;; 			   name))
      ;; 	(return entry))
      (when (== 0 (funcall strncmp name
      			   (aref builtin-name entry)
      			   (calc-builtin-name-max-len *builtin-declaration*)))
      	(return entry))
      (inc entry))
    (return (cl:length *builtin-declaration*))))
(%function lookupmin ((uintgr idx)) -> int
  (dcomment "Return the minimum number of arguments for a builtin function with index IDX.")
  (comment "(void) name;" :prefix "") ;; FIXME
  (return (aref builtin-par-min idx)))
(%function lookupmax ((uintgr idx)) -> int
  (dcomment "Return the maximum number of arguments for a builtin function with index IDX.")
  (comment "(void) name;" :prefix "")
  (return (aref builtin-par-max idx)))
(%function lookupfn ((uintgr idx)) -> fn_ptr_type
  (dcomment "Given an index IDX of a builtin function return the corresponding function pointer.")
  (return (aref builtin-fptr idx)))
(%function _apply ((o function)
		  (o args)
		  (o* env)) -> o
  (when (== *symbol* (cons-type function))
    (decl ((uintgr name (cons-name function))
	   (int nargs (funcall listlength args)))
      (when (< (cl:length *builtin-declaration*) name)
	(%err "not a function"))
      (when (< nargs (funcall lookupmin name))
	(%err "too few args"))
      (when (< (funcall lookupmin name) nargs)
	(%err "too many args"))
      (return (funcall
	       (cast 'fn_ptr_type
		     (funcall lookupfn name))
	       args *env))))
  (when (and (%listp function)
	     (funcall issymbol (%car function)
		      (get-builtin-idx-from-name 'lambda)))
    (set function (%cdr function))
    (decl ((o
	    result
	    (funcall closure 0
		     NULL NULL function args env)))
      (return (funcall _eval result *env))))
  (when (and (%listp function)
	     (funcall issymbol (%car function)
		      (get-builtin-idx-from-name 'closure)))
    (set function (%cdr function))
    (decl ((o
	    result
	    (funcall closure 0
		     NULL
		     (%car function)
		     (%cdr function)
		     args env)))
      (return (funcall _eval result *env))))
  (%err "illegal function")
  (return NULL))

(%function carx ((o arg)) -> o
  (dcomment "Car with argument check. Throws error, when argument isn't a list.")
  (when (== 0 (%listp arg))
    (%err "can't take car"))
  (when (== cnil arg)
    (return cnil))
  (return (%car arg)))
(%function cdrx ((o arg)) -> o
  (dcomment "Cdr with argument check. Throws error, when argument isn't a list.")
  (when (== 0 (%listp arg))
    (%err "can't take cdr"))
  (when (== cnil arg)
    (return cnil))
  (return (%cdr arg)))

(%function _eval ((o form)
		 (o env)) -> o
  (dcomment "eval")
  ;; (%puts "eval ")
  ;; (%puts "form ")
  ;; (funcall _print-object form)
  ;; (funcall _putchar #\Newline)
  ;; (%puts "env ")
  ;; (funcall _print-object env)
  ;; (funcall _putchar #\Newline)
  (decl ((int TC 0))
    (comment "EVALJUMP:" :prefix "") ;; FIXME this is crazy
    (when (< freespace 10)
      (funcall gc form env))
    ;; FIXME i left out some _end stuff and serial break
    (when (== NULL form)
      (comment "NULL")
      (return cnil))
    (when (== *number* (cons-type form)) (comment "number")
      
      ;(%puts "number")
      (return form))
    (when (== *symbol* (cons-type form)) (comment "symbol")
      (decl ((uintgr name (cons-name form)))
	(when (== (get-builtin-idx-from-name 'nil)
		  name)
	  (comment "nil")
	  ;;(%puts "not builtin")
	  (return cnil))
	(decl ((o pair (funcall value name env)))
	  (when (!= NULL pair) (comment "sym cdr pair")
	    ;;(%puts "sym cdr pair")
	    (return (%cdr pair)))
	  (set pair (value name global-env))
	  (if (!= NULL pair)
	      (progn (comment "sym cdr pair in global env")
		     ;;(%puts "sym cdr pair")
		     (return (%cdr pair)))
	      (if (<= name (cl:length *builtin-declaration*))
		  (progn (comment "builtin declaration")
			 ;;(%puts "form")
			 (return form))
		  (progn
		    (funcall puti name)
		    (funcall _print-object form )
		    (%err "undefined variable")))))))
    (comment "it's a list")
    (decl ((o function (%car form))
	   (o args (%cdr form)))
      
      (when (== *symbol* (cons-type function)) (comment "list starting with symbol")
	(decl ((uintgr name (cons-name function)))
	  
	  (when (== (get-builtin-idx-from-name 'let) 
		    name) (comment "let")
	    ;; FIXME leaving out LETSTAR
	    
	    (decl ((o assigns (%car args))
		   (o forms (%cdr args))
		   (o newenv env))
	      (comment "process LET")
	      ;;(%puts "process let")
	      (%dolist (assign assigns) (comment "go through assigns")
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
	  
	  (when (== (get-builtin-idx-from-name 'lambda) name)
	    (comment "process LAMBDA")
	    (when (== NULL env) (comment "lambda nil env")
	      (return form))
	    (decl ((o envcopy NULL))
	      (%dolist (pair env) 
		(decl ((o val (%cdr pair)))
		  (when (== *number* (cons-type val)) (comment "number")
		    (set val (funcall _number
				      (cons-integer val))))
		  (%push (funcall _cons (%car pair)
				  val)
			 envcopy)))
	      (progn (comment "call lambda")
		     (return (funcall
			      _cons
			      (funcall
			       _symbol
			       (get-builtin-idx-from-name
				'closure))
			      (funcall _cons envcopy args))))))
	  
	  (when (is-idx-in-type-range name special)
	    (comment "process SPECIAL form")
	    (return (funcall (cast 'fn_ptr_type (funcall lookupfn name))
			     args env)))
	  (when (is-idx-in-type-range name tailrec)
	    (comment "process TAIL CALL form")
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
	    (%dolist (e form) (comment "count number of arguments")
	      (decl ((o obj (funcall _cons
				     (funcall _eval e env)
				     NULL)))
		(set (%cdr tail) obj)
		(set tail obj)
		(inc nargs)))
	    (set function (%car head))
	    (set args (%cdr head))
	    (when (== *symbol* (cons-type function)) (comment "function of type symbol")
	      (decl ((uintgr name (cons-name function)))
		(when (<= (cl:length *builtin-declaration*) name)
		  (comment "name is not a bultin")
		  (%err "not a function"))
		(when (< nargs (funcall lookupmin name))
		  (%err "too few args"))
		(when (< (funcall lookupmax name) nargs)
		  (%err "too many args"))
		(decl ((o result (funcall
				  (cast 'fn_ptr_type
					(funcall lookupfn name))
				  args env)))
		  (%pop gc-stack)
		  (progn (comment "symbol")
			 (return result)))))
	    (when (and (%listp function)
		       (funcall issymbol (%car function)
				(get-builtin-idx-from-name 'lambda)))
	      (comment "listp function and (car function) is lambda")
	      (set form (funcall closure TCstart
				 fname NULL (%cdr function) args
				 (addr-of env)))
	      (%pop gc-stack)
	      (set TC 1)
	      (dcomment "goto eval2")
	      (comment "goto EVALJUMP;" :prefix ""))
	    (when (and (%listp function)
		       (funcall issymbol (%car function)
				(get-builtin-idx-from-name 'closure)))
	      (comment "listp function and (car function) is closure")
	      (set function (%cdr function))
	      (set form (funcall closure TCstart
				 fname (%car function) (%cdr function) args
				 (addr-of env)))
	      (%pop gc-stack)
	      (set TC 1)
	      (dcomment "goto eval3")
	      (comment "goto EVALJUMP;" :prefix ""))
	    (%err "illegal func")
	    (progn (comment "eval returns nil")
		   (return cnil))))))))
(%function init-env () -> void
  (dcomment "Clear globale environment, initialize TEE to be the symbol that points to the TEE symbol number.")
  (set global-env NULL)
  (set tee (funcall _symbol (get-builtin-idx-from-name 'tee))))

(%function _getc () -> int
  (dcomment "If lastchar is 0, blocking read of a character, followed by a print. If lastchar is not 0, return its value and clear it.")
  (when last-char
    (decl ((int temp last-char))
      (set last-char 0)
      (return temp)))
  ;; (decl ((static int idx 0)
  ;; 	 (int temp (aref cmd idx)))
  ;; 	(inc idx)
  ;; 	(funcall printf "%c" temp)
  ;; 	(return temp))
  (decl ((int temp (funcall _getchar)))
    (funcall _putchar temp)
    (return temp)))
(%function _isspace ((int c)) -> int
  (if (or (== c #\Space) ;; FIXME i never saw vertical tab
	    (== c #\Newline)
	    (== c #\Tab)
	    (== c #\Return)
	    (== c #\Page)
	    (== c #\Linefeed))
      (return 1)
      (return 0)))
(%function nextitem () -> o
  (dcomment "Get characters from the input stream and tokenize. Handles whitespace, comments, parenthesis, dot, numbers, builtins and user functions.")
  (decl ((int ch (funcall _getc)))
    (while (funcall _isspace ch)
      (set ch (funcall _getc)))
    (when (== #\; ch) (comment "; initializes comment until next opening paren")
      (while (!= #\( ch)
	(set ch (funcall _getc)))
      (set ch #\())
    (when (== #\newline ch)
      (set ch (funcall _getc)))
    #+stdio (when (== EOF ch)
	      (funcall exit 0))
    #-stdio (when (== -1 ch)
	      (%puts "EXIT"))
    (when (== #\) ch)
      (comment "ket") 
      (return (cast 'o *ket*))) 
    (when (== #\( ch)
      (comment "bra")
      (return (cast 'o *bra*)))
    (when (== (char-code #\') ch)
      (comment "quo")
      (return (cast 'o *quo*)))
    (when (== #\. ch)
      (comment "dot")
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
				     (%err "illegal char after #"))))
			 (set ch (funcall _getc))))))
      (decl ((intgr isnumber (< (funcall digitvalue ch) base)))
	(comment "in case var is one letter")
	(set (aref buffer 2) 0)
	(while (and (== 0 (funcall _isspace ch))
		    (!= #\) ch)
		    (!= #\( ch)
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
	(when (== #\( ch)
	  (set last-char #\())
	(when isnumber
	  (if (and (== base 10)
		   (< (+ (cast 'uintgr 32767)
			 (/ (- 1 sign)
			    2))
		      result))
	      (%err "num out of range"))
	  (return (funcall _number (* sign result))))
	(decl ((intgr x (funcall builtin buffer)))
	  ;(funcall puti x)
	  (when (== x (get-builtin-idx-from-name 'nil)) (comment "cnil")
	    (return cnil))
	  (if (< x (cl:length *builtin-declaration*))
	      (progn (comment "builtin symbol")
		     (return (funcall _symbol x)))
	      (progn (comment "usersymbol")
		     (return (funcall _symbol (funcall pack40 buffer))))))))))
(%function read-rest () -> o
  (dcomment "FIXME: Part of the tokenizer of the input stream. I think it reads lists.")
  (decl ((o item (funcall nextitem)))
    (when (== (cast 'o *ket*) item) (comment "ket")
      (return NULL))
    (when (== (cast 'o *dot*) item) (comment "dot")
      (decl ((o arg1 (funcall _read)))
	(if (!= NULL (funcall read-rest))
	    (%err "malformed list"))
	(return arg1)))
    (when (== (cast 'o *quo*) item) (comment "quo")
      (decl ((o arg1 (funcall _read)))
	(return (funcall
		 _cons
		 (funcall
		  _cons
		  (funcall _symbol
			   (get-builtin-idx-from-name 'quote))
		  (funcall _cons arg1 NULL))
		 (funcall read-rest)))))
    (when (== (cast 'o *bra*) item) (comment "bra")
      (set item (funcall read-rest)))
    (return (funcall _cons item (funcall read-rest)))))
(%function _strlen ((const char* s)) -> int
  (dcomment "Like strlen but limited to maximum name length.")
  (decl ((const char* start s))
    (for ((int i 0) (and *s
			 (< i (calc-builtin-name-max-len *builtin-declaration*)))
	  (inc i))
      (inc s))
    (return (- s start))))
(%function _print-object ((o form)) -> void
  (dcomment "print-object")
  (if (== NULL form)
      (%puts "nil")
      (if (and (%listp form)
	       (funcall issymbol (%car form)
			(get-builtin-idx-from-name 'closure)))
	  (%puts "<closure>")
	  (if (%listp form)
	      (progn
		(funcall _putchar (cast 'int #\())
		(funcall _print-object (%car form))
		(set form (%cdr form))
		(while (and (!= cnil form)
			    (%listp form))
		  (funcall _putchar #\Space)
		  (funcall _print-object (%car form))
		  (set form (%cdr form)))
 		(if (!= NULL form)
		    (progn (%puts " . ")
			   (funcall _print-object form)))
		(funcall _putchar #\)))
	      (if (== *number* (cons-type form))
		  (funcall puti (funcall _integer form))
		  (if (== *symbol* (cons-type form))
		      (decl ((char* s (funcall name form))
			     (int len (funcall _strlen s)))
		       (funcall _putsn s len))
		      (%err "print err")))))))
(%function _read () -> o
  (dcomment "Read either a list, cons, quotes, or any tokens the function nextitem returned.")
  (decl ((o item (funcall nextitem)))
    (when (== (cast 'o *bra*) item) (comment "bra")
      (return (funcall read-rest)))
    (when (== (cast 'o *dot*) item) (comment "dot")
      (return (funcall _read)))
    (when (== (cast 'o *quo*) item) (comment "quo")
      (return (funcall _cons
		       (funcall _symbol
				(get-builtin-idx-from-name 'quote))
		       (funcall _cons (funcall _read) NULL))))
    (return item)))
(%function repl ((o env)) -> void
  (dcomment "repl")
  (for (() () ())
    (funcall gc NULL env)
    (%puts "freespace=")
    (funcall puti freespace)
    (funcall _putchar #\Newline)
    (%puts "> ")
    (decl ((o line (funcall _read)))
      (when (== cnil line)
	(funcall _putchar #\Newline)
	(return))
      (funcall _putchar #\Newline)
      (%push line gc-stack)
      (funcall _print-object (funcall _eval line env))
      (%pop gc-stack)
      (funcall _putchar #\Newline)
      (funcall _putchar #\Newline))))
(%function _putsn ((char* string) (int len)) -> void
  (funcall write 0 string len)
  ;; (funcall puts string)
  )
(%function _putchar ((int c)) -> void
  (funcall write 0 (addr-of c) 1)
  ;; (funcall putchar c)
  )
(%function _getchar () -> int
  ;; (return (funcall getchar))
  (decl ((char b))
    (decl ((int num (funcall read 0 (addr-of b) 1)))
      (if (< num 1)
	  (return -1)
	  (return (cast 'int b)))))
  )
(%function puti ((intgr i)) -> void
  (dcomment "output integer")
  (when (< i 0)
    ;; (funcall SciaXmit (char-code #\-)) 
    (funcall _putchar (char-code #\-))
    (comment "output minus (-) character")
    (set i (- i)))
  (decl ((intgr rev 0)
	 (intgr digits 0))
    (while (< 0 i)
      (decl ((int digit (% i 10)))
	(set rev (+ (* 10 rev) digit))
	(set i (/ i 10))
	(inc digits)))
    (if (== 0 digits)
	(funcall _putchar #\0)
	(for ((intgr j 0) (< j digits) (inc j))
	  (decl ((int digit (% rev 10)))
	    (funcall _putchar (+ 48 digit))
	    (set rev (/ rev 10)))))))

(%function putui ((uintgr i)) -> void
  (dcomment "output unsigned integer")
  (decl ((uintgr rev 0)
	 (uintgr digits 0))
    (while (< 0 i)
      (decl ((uintgr digit (% i 10UL)))
	(set rev (+ (* 10UL rev) digit))
	(set i (/ i 10UL))
	(inc digits)))
    (if (== 0 digits)
	(funcall _putchar #\0)
	(for ((uintgr j 0) (< j digits) (inc j))
	  (decl ((uintgr digit (% rev 10UL)))
	    (funcall _putchar (+ 48 digit))
	    (set rev (/ rev 10)))))))
	
