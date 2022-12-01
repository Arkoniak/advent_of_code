(defun input (fname)
          (uiop:read-file-lines fname))

(defun move (p dir)
  (destructuring-bind (d v) (uiop:split-string dir)
    (destructuring-bind (x y aim) p
      (let ((v (parse-integer v)))
        (if (string= d "forward")
          (list (+ v x) (+ (* aim v) y) aim)
          (if (string= d "up")
            (list x y (- aim v))
            (list x y (+ aim v))))))))

;; Result
(apply #'* (subseq (reduce #'move (input "input.txt") :initial-value '(0 0 0)) 0 2))

;; Test
(apply #'* (subseq (reduce #'move (input "test_input.txt") :initial-value '(0 0 0)) 0 2))

;; Misc
(move '(0 0 2) "forward 2")
(move '(0 0 2) "up 1")
(move '(0 0 2) "down 1")

