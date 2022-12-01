;; Not needed
;; (ql:quickload "str")
;; (ql-quickload "cl-lex")

;; Used in code
;; (ql:quickload :split-sequence)

(require "split-sequence")

(defstruct bingo
  nums
  cards)

(defun read-matrix (lst)
  (let* ((m (mapcar (lambda (x) 
                      (mapcar #'parse-integer 
                              (split-sequence:split-sequence #\Space x
                                                             :remove-empty-subseqs t)))
                    lst))
         (n1 (length m))
         (n2 (length (first m))))
    (make-array (list n1 n2) :initial-contents m)))

(defun input (filename)
    (let ((lst (split-sequence:split-sequence "" 
                                              (uiop:read-file-lines filename) 
                                              :test #'equal)))
      (make-bingo :nums (mapcar #'parse-integer 
                                (split-sequence:split-sequence #\, (car (car lst))))
                  :cards (mapcar #'read-matrix (cdr lst)))))

;; (defun input (filename)
;;   (labels ((read-matrix 
;;                (lst)
;;              (let* ((m (mapcar (lambda (x) 
;;                                  (mapcar #'parse-integer 
;;                                          (split-sequence:split-sequence #\Space x
;;                                                                         :remove-empty-subseqs t)))
;;                                lst))
;;                     (n1 (length m))
;;                     (n2 (length (first m))))
;;                (make-array (list n1 n2) :initial-contents m))))
;;     (let ((lst (split-sequence:split-sequence "" 
;;                                               (uiop:read-file-lines filename) 
;;                                               :test #'equal)))
;;       (make-bingo :nums (mapcar #'parse-integer 
;;                                 (split-sequence:split-sequence #\, (car (car lst))))
;;                   :cards (mapcar #'read-matrix (cdr lst))))))

;; (defun input (filename)
;;   (labels ((read-matrix 
;;                (lst)
;;              (let* ((m (mapcar (lambda (x) 
;;                                  (mapcar #'parse-integer 
;;                                          (split-sequence:split-sequence #\Space x
;;                                                                         :remove-empty-subseqs t)))
;;                                lst))
;;                     (n1 (length m))
;;                     (n2 (length (first m))))
;;                (make-array (list n1 n2) :initial-contents m))))
;;     (let ((lst (split-sequence:split-sequence "" 
;;                                               (uiop:read-file-lines filename) 
;;                                               :test #'equal)))
;;       (make-bingo :nums (mapcar #'parse-integer 
;;                                 (split-sequence:split-sequence #\, (car (car lst))))
;;                   :cards (mapcar #'read-matrix (cdr lst))))))

(defun mark-if (m v)
  (dotimes (i (array-total-size m))
    (if (= (row-major-aref m i) v)
        (setf (row-major-aref m i) -1))) 
  m)

(defun gen-iterator (m i j di dj)
    (let ((i (- i di))
          (j (- j dj)))
      #'(lambda ()
          (setq i (+ i di) j (+ j dj))
          (let* ((dims (array-dimensions m))
                 (m1 (first dims))
                 (m2 (second dims)))
            (if (or (< i 0) (< j 0) (>= i m1) (>= j m2))
              nil
              (aref m i j))))))

(defun total (m)
  (let ((res 0))
    (dotimes (i (array-total-size m))
      (let ((x (row-major-aref m i)))
        (if (> x 0)
            (setf res (+ x res)))))
    res))

;; Misc
(defparameter *lst* (uiop:read-file-lines "test_input.txt"))
(defparameter *data* (input "test_input.txt"))

(subst-if 1 (lambda (x) (print x)) (bingo-cards *data*))

(mapcar (lambda (x) (mark-if x 7)) (bingo-cards *data*))

(defun gen-iterator (m i j di dj)
    (let ((i (- i di))
          (j (- j dj)))
      #'(lambda ()
          (setq i (+ i di) j (+ j dj))
          (let* ((dims (array-dimensions m))
                 (m1 (first dims))
                 (m2 (second dims)))
            (if (or (< i 0) (< j 0) (>= i m1) (>= j m2))
              nil
              (aref m i j))))))

(setf *yyy* (gen-iterator (first (bingo-cards *data*)) 0 0 0 1))

(funcall *yyy*)

(null (funcall *yyy*))
(total (first (bingo-cards *data*)))

(first (bingo-cards *data*))

(let ((i 1)
      (j 2))
    (setq i (+ i 1) j (+ j 2))

    (list i j)
  )

(defun gen-counter ()
    (let ((count 0))
        #'(lambda () (incf count))
      )
  )

(defvar *xxx* (gen-counter))
(funcall *xxx*)

(first (bingo-cards *data*))

(my-mapcar* #'mark-if (list (bingo-cards *data*) 7))

(defun my-mapcar* (func arglist)
  (mapcar 
     (lambda (args)
       (apply func args))
     arglist))

(bingo-cards *data*)
  (let ((dims (array-dimensions m)))
    (dotimes (i (first dims))
      (dotimes (j (second dims))
        (let ((x (aref m i j)))
          (if (= x v)
            (setf (aref m i j) -1)) 
          ))))
  m)

(mark-if #2A((1 2) (2 4)) 2)

(subst 1 0 (make-array '(2 3)))
(subst 1 0 (list '(0 0 0) '(0 0 0)))

(let* ((x (list '(0 0 0) '(0 0 0)))
    (y (setf (nth 0 (nth 1 x)) 1)))
  x
  )

(map #'array #'identity (make-array '(2 3)))

(print "asd")

(mapcar #'parse-integer 
        (split-sequence:split-sequence #\Space "  1 2 3 4    5  "
                                       :remove-empty-subseqs t))

(read-string "  1 2 3 4    5  ")

(mapcar (lambda (x) (not (= 0 (length x)))) *lst*)

            ;;(make-array (list n1 n2) :initial-contents m)
(n1 (length m))
(n2 (length (first m))))

(defun read-matrix (lst)
  (let* ((m (mapcar (lambda (x) 
                      (mapcar #'parse-integer 
                              (split-sequence:split-sequence #\Space x
                                                             :remove-empty-subseqs t)))
                    lst))
         (n1 (length m))
         (n2 (length (first m))))
    (make-array (list n1 n2) :initial-contents m)))



(let ((x (split-sequence:split-sequence "" *lst* :test #'equal)))
 (split-sequence:split-sequence #\, (first (first x)))
  )

  (mapcar #'read-matrix 
          (cdr 
            (split-sequence:split-sequence "" *lst* :test #'equal))))

(let* ((m (mapcar (lambda (x) 
                    (mapcar #'parse-integer 
                            (split-sequence:split-sequence #\Space x
                                                           :remove-empty-subseqs t)))
                  (second (split-sequence:split-sequence "" *lst*
                                                         :test #'equal))))
       (n1 (length m))
       (n2 (length (first m)))
       )
  (make-array (list n1 n2) :initial-contents m))

(uiop:split-string (nth 2 *lst*) :separator " ")
(cl-ppcre:split "\\s+" (nth 3 *lst*))

(defun recur-input (lines )

  )


(defun read-string (s)
  (mapcar #'parse-integer 
          (remove-if 
            (lambda (x) (= 0 (length x))) 
            (uiop:split-string s :separator " ")))
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

(let* ((x (list '(1 2 3) '(4 5 6)))
        (n1 (length x))
        (n2 (length (first x)))
        (y (make-array (list n1 n2) :initial-contents x))
        (xxx (setf (aref y 0 0) 10))
       )
  y
  )

(defparameter *a1*
 #2A((1 2 3 4) (5 6 7 8)))

(define-peg-rule digit ()
  [0-9])
(peg-parse (number sign digit (* digit))
           (sign   (or "+" "-" "")))
(split-sequence:split-sequence "" (uiop:read-file-lines "test_input.txt") :test #'equal)

(cons 'a 'b)
(car (cons 'a 'b))
(cdr (cons 'a 'b))
