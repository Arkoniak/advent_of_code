(defun result (v)
      (* (reduce (lambda (x y) (+ (* 2 x) y)) v)
         (reduce (lambda (x y) (+ (* 2 x) (- 1 y))) v :initial-value 0)))

(defun part1 (lst)
    (let ((lst2 
            (map 'list (lambda (x) (map 'list (lambda (y) (if (eq y #\0) -1 1)) x)) lst)))
      (result
       (mapcar (lambda (x) (if (> x 0) 1 0))
          (apply #'mapcar #'+ lst2)))))

;; Result
(part1 (uiop:read-file-lines "input.txt"))

;; Test
(part1 (uiop:read-file-lines "test_input.txt"))
