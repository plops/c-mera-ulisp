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
(defun bla (a) (setq a (add a -1)) (if (less 0 a) 1 0))
(bla 2)


;; list null dolist assoc
(defvar md nil)

(defun adr (frm to tim) ;; add road
  (push (list frm to tim) md)
  (push (list to frm tim) md))

(defun adi (i q) ;; add item to queue
  (if (null q) 
      (cons i q)
    (if (< (first i) (car (car q)))
        (cons i q)
      (cons (first q) (adi i (cdr q))))))

(defun adq (tim loc via pq) ;; add to queue
  (setq pq (adi (list tim loc via) pq))
  pq)

(defun ars (loc go pq) ;; add roads
  (dolist (i md pq)
    (let* ((frm (first i))
           (to (second i))
           (tim (third i)))
      (when (and (eq frm loc) (not (assoc to vis)))
	(setq pq (adq (add go tim) to loc pq))))))

(defun gro (frm to)
  (let* ((vis (list (cons frm nil)))
         (pq (ars frm 0 nil))
         w)
    (loop
     (when (eq frm to) (return (reverse vis)))
     (unless pq (return))
     (setq w (first pq))
     (setq frm (second w))
     (setq pq (cdr pq))
     (unless (assoc frm vis)
       (setq vis (cons (cons frm (third w)) vis))
       (setq pq (ars frm (car w) pq))))))
