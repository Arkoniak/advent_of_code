(defun input (fname)
  (mapcar (lambda (y) (map 'vector (lambda (x) (if (eq x #\0) -1 1)) y))
         (uiop:read-file-lines fname)))

(defun todec (vec)
    (reduce (lambda (x y) (+ (* 2 x) (if (< y 0) 0 1))) vec :initial-value 0))

(defun filter (func lst n)
    (if (= (length lst) 1)
        (todec (first lst))
        (let* ((v0 (reduce 
                     (lambda (x y) (+ (svref y n) x))
                     lst :initial-value 0))
                (v (funcall func v0))
                (lst2 (remove-if-not (lambda (x) (= v (svref x n))) lst)))
          (filter func lst2 (+ n 1)))))

(defun part2 (fname)
  (let ((lst (input fname)))
    (*   
      (filter (lambda (x) (if (< x 0) -1 1)) lst 0)
      (filter (lambda (x) (if (>= x 0) -1 1)) lst 0))))

;; Result
(part2 "input.txt")

;; Test
(part2 "test_input.txt")

;; Misc

(uiop:read-file-lines "test_input.txt")


;; ok, we should check that recursion converge, but we believe in task master
(defun filter (func lst n)
    (if (= (length lst) 1)
        (first lst)
        (let* ((v0 (reduce 
                     (lambda (x y) (+ (svref y n) x))
                     lst :initial-value 0))
               (v (funcall func (v0))))
          (remove-if-not (lambda (x) (= v (svref x n))) lst)
          )
      )
  )

(filter (lambda (x) (if (< x 0) -1 1)) (input "test_input.txt") 0)
(filter (lambda (x) (if (>= x 0) -1 1)) (input "test_input.txt") 0)



(setf l (filter (lambda (x) (if (< x 0) -1 1)) (input "test_input.txt") 0))
(filter (lambda (x) (if (< x 0) -1 1)) l 1)

(remove-if-not #'oddp '(1 2 3 4))

(let* ((x 1)
      (y (+ x 1)))
    (list :x x :y y)
  )

(first '(1 2 3))
(mapcar (lambda (x) (svref x 0)) (input "test_input.txt"))
(remove-if (lambda (x) (/= -1 (svref x 0))) (input "test_input.txt"))

(nth 1 '(3 5 7))

(svref (vector 10 20 30 40) 2)
(aref (vector 10 20 30 40) 2)

(defun tst (func x)
    (funcall func (+ x 1)))

(tst (lambda (x) (* x x)) 1)
