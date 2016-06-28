(defun tak (x y z)
  (if (not (less y x))
      z
      (tak
       (tak (add x -1) y z)
       (tak (add y -1) z x)
       (tak (add z -1) x y))))

(tak 18 12 6)

