(defun input (fname)
  (mapcar #'parse-integer
          (uiop:read-file-lines fname)))

(defun comp (x y)
    (if (null x)
      0
      (if (< x y) 1 0)))

(defun recur (lst num acc)
  (if (null lst)
    acc
    (let ((rlst (cdr lst))
          (x (car lst)))
      (recur rlst x (cons (comp num x) acc)))))

;; Result
(reduce #'+ (recur (input "input.txt") nil '()))

; Test solution
(reduce #'+ (recur (input "test_input.txt") nil '()))
