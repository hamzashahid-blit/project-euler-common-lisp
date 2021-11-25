;;;; All main comments common to both files are in main.lisp

;;;; "-n" suffix is for "new" functions using libraries

(defpackage euler-cl
  (:use :cl :str :iterate :cl-punch)) ; :gtwiwtg 
(in-package :euler-cl)

(enable-punch-syntax)

(defun p1-n (n)
  (iter (for x below n)
        (when (or (zerop (mod x 3))
                  (zerop (mod x 5)))
          (sum x))))

;;------------------------------------------------------

(defun primes-n (n)
  (iter (for x from 2 to (sqrt n))
    (when (and (zerop (mod n x))
               (null (primes-n x)))
      (collect x))))

;; Accurate prime factors
(defun acc-primes-n (n)
  (iter (for x from 2 to (1- n))
    (when (and (zerop (mod n x))
               (null (primes-n x)))
      (collect x))))

(defun prime? (n)
  (iter (for x from 2 to (sqrt n))
        (when (zerop (mod n x))
          (return nil))
        (finally (return t))))

(defun p7-n (n)
  (car
    (gtwiwtg:pick-out `(,(1- n))
      (gtwiwtg:filter! #'prime? (gtwiwtg:range :from 2)))))

;;------------------------------------------------------

(defun p8 ()
  ;; convert number to a string without newlines
  (let* ((num (remove #\newline "
73167176531330624919225119674426574742355349194934
96983520312774506326239578318016984801869478851843
85861560789112949495459501737958331952853208805511
12540698747158523863050715693290963295227443043557
66896648950445244523161731856403098711121722383113
62229893423380308135336276614282806444486645238749
30358907296290491560440772390713810515859307960866
70172427121883998797908792274921901699720888093776
65727333001053367881220235421809751254540594752243
52584907711670556013604839586446706324415722155397
53697817977846174064955149290862569321978468622482
83972241375657056057490261407972968652414535100474
82166370484403199890008895243450658541227588666881
16427171479924442928230863465674813919123162824586
17866458359124566529476545682848912883142607690042
24219022671055626321111109370544217506941658960408
07198403850962455444362981230987879927244284909188
84580156166097919133875499200524063689912560717606
05886116467109405077541002256983155200055935729725
71636269561882670428252483600823257530420752963450"))

          ;; make a list of num's digits
          (digits (iter (for i in-string num)
                        (collect (parse-integer (string i))))))

    ;; get the max of looping our list of digits in pairs of 13
    ;; (apply #'max
    ;;   (loop :for (a b c d e f g h i j k l m) :on digits :while m
    ;;     :collect (* a b c d e f g h i j k l m)))

    (iter (for (a b c d e f g h i j k l m) on digits while m)
      (collect (* a b c d e f g h i j k l m)))

    ;; SLOWER!!!
    (iter (for digitff :initially digits :then (cdr digitff))
      (until (< (length digitff) 13))
      (maximize (apply #'* (subseq digitff 0 13))))))
 
;; TODO for p9:
;; 1. Fix ugly let
;; 2. make neater
;; 3. prevent: a = 2, b = 3 -> a = 3, b = 2 being 2 iterations.
;; in other words, avoid duplicate calculation of (c^2)
(defun p9 ()
  (iter outer (for a from 1 to 1000)
    (iter (for b from 1 to 1000)
      (let ((c (sqrt (+ (sqr a) (sqr b)))))
        (when (= (+ a b c) 1000)
          (format t "~a^2 + ~a^2 = ~a^2~%" a b c)
          (return-from outer (* a b c)))))))
