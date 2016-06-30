(defun tak (x y z)
  (princ x)
  (if (not (less y x))
      z
      (tak
       (tak (add x -1) y z)
       (tak (add y -1) z x)
       (tak (add z -1) x y))))

(tak 18 12 6)

(defun bla (a)
  (princ a)
  (setq a (add a -1))
  (if (less 0 a)
      (bla a)
      0))
(bla 2)

(defun bla (a) (princ a) (setq a (add a -1)) (if (less 0 a) 1 0))
(bla 2)

