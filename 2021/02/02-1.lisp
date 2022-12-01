(defun input (fname)
          (uiop:read-file-lines fname))

(defun move (p dir)
  (destructuring-bind (d v) (uiop:split-string dir)
    (let ((v (parse-integer v)))
      (if (string= d "forward")
        (mapcar #'+ (list v 0) p)
        (if (string= d "up")
            (mapcar #'+ (list 0 (- v)) p)
            (mapcar #'+ (list 0 v) p))))))

;; Result
(apply #'* (reduce #'move (input "input.txt") :initial-value '(0 0)))

;; Test
(apply #'* (reduce #'move (input "test_input.txt") :initial-value '(0 0)))
