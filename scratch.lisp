(in-package :cl-user)
(defun toradix40 (ch)
  (when (= 0 ch)
    (return-from toradix40 0))
  (when (<= (char-code #\0) ch (char-code #\9))
    (return-from toradix40 (+ 30 (- ch (char-code #\0)))))
  (setf ch (logior ch #x20))
  (when (<= (char-code #\a) ch (char-code #\z))
    (return-from toradix40 (+ 1 (- ch (char-code #\a)))))
  nil
  
  ;(break "illegal character")
  )
(defun fromradix40 (n) 
  "Convert a radix 40 character into an ascii character."
  (when (and (<= 1 n) (<= n 26))
    (return-from fromradix40 (+ n (char-code #\a) -1)))
  (when (and (<= 1 30) (<= n 39))
    (return-from fromradix40 (+ n (char-code #\0) -30)))
  (when (= 27 n)
    (return-from fromradix40 (char-code #\-)))
  (return-from fromradix40 0))

(defun radix40-name-to-string (x)
  (mapcar #'code-char
   (loop for n from 2 downto 0 collect
	(prog1
	    (fromradix40 (mod x 40))
	  (setf x (floor x 40))))))


#+nil
(radix40-name-to-string 7012)


(length
 (loop for i below 200 when (toradix40 i) collect
      (list  i (code-char i) (toradix40 i))))

(let ((a (loop for i below 200 when (toradix40 i) collect
	      (list  i (code-char i) (toradix40 i)))))
  (loop for i below 3 do
       (loop for j below 11 ;(length a)
	  do
	    (format t "~4a" (elt (elt a j) i)))
       (terpri))
  (terpri)
  (loop for i below 3 do
       (loop for j from 11 below (+ 11 26)	;(length a)
	  do
	    (format t "~4a" (elt (elt a j) i)))
       (terpri))
  (terpri)
  (loop for i below 3 do
       (loop for j from (+ 11 26) below (length a)
	  do
	    (format t "~4a" (elt (elt a j) i)))
       (terpri)))


(defun digitvalue (d)
  (declare (type (unsigned-byte 8) d)
	   (values fixnum &optional))
  (if (<= (char-code #\0) d (char-code #\9))
      (- d (char-code #\0))
      (progn (setf d (logior d #x20))
	     (if (<= (char-code #\a) d (char-code #\f))
		 (+ 10 (- d (char-code #\a)))
		 16))))

(defparameter *digitval-ascii*
 (loop for i below 256 collect
      (list (digitvalue i) (code-char i) )))

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

(defun puti (i)
  (when (< i 0)
    (format t "-")
    (setf i (- i)))
  (let ((rev 0))
    (loop while (< 0 i) do
      (let ((digit (mod i 10)))
	(setf rev (+ (* 10 rev) digit))
	(setf i (floor i 10))))
    (loop while (< 0 rev) do
      (let ((digit (mod rev 10)))
	(format t "~c" (code-char (+ 48 digit)))
	(setf rev (floor rev 10))))))

(loop for i in (list -123 -100 -1 0 1 2 3 100) do
     (progn
       (format t "~d " i)
       (puti i)
       (terpri)))

;; -123 -123
;; -100 -1
;; -1 -1
;; 0 
;; 1 1
;; 2 2
;; 3 3
;; 100 1
