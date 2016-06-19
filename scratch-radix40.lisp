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
