(car (uiop:read-file-lines "test_input.txt"))

(defun recur-input (lines )

  )

(defstruct bingo
  nums
  cards
  )

(bingo-nums (make-bingo :nums 1 :cards 2))
(apply #'map 'list (lambda (&rest x) (apply 'vector x)) (list #(1 2) #(3 4)))

(setf (nth 1 #(1 2)) 3)
(nth 1 #(1 2))
(vector '(1 2))
(describe #'cl-labels)

(listp '(1 2 3))
(defun mapmap (f lst)
  (if (listp lst)
    (mapcar (lambda (x) (mapmap f x)) lst)
    (funcall f lst)))

(mapmap (lambda (x) (if (= x 2) -1 x)) (list '(1 2) '(3 4)))

(let ((x (acons :b 2 (acons :a 1 '()))))
  (assoc :a x)
  )

(let ((lst (list (list 1 2 3) (list 3 4 5))))
    (subst 0 3 lst)
  )

(setq tree1 '(1 (1 2) (1 2 3) (1 2 3 4)))
(atom tree1)
(subst-if 5 (lambda (x) (and (atom x) (>= x 3))) tree1)

(and (= 1 1) (= 2 3))
