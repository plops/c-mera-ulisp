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

(loop for i below 200 when (toradix40 i) collect
     (format nil "~c ~2,'0d ~2,'0d"  (code-char i) i (toradix40 i)))
