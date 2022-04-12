;;;; All main comments common to both files are in main.lisp

;;;; "-n" suffix is for "new" functions using libraries

(defpackage euler-cl
  (:use :cl :iterate) ; :gtwiwtg :cl-punch :str 
  (:local-nicknames (:g :gtwiwtg)))
;; (:shadowing-import-from :iterate)

(in-package :euler-cl)

(cl-punch:enable-punch-syntax)

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
  (iter (for x from 2 to (1- n)) ; (sqrt n)
        (when (zerop (mod n x))
          (return nil))
        (finally (return t))))

(defun p7-n (n)
  (car (g:pick-out (list (1- n))
         (g:filter! #'prime? (g:range :from 2)))))

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

    ;; (iter (for (a b c d e f g h i j k l m) on digits)
    ;;   (while m)
    ;;   (collect (* a b c d e f g h i j k l m)))

    ;; SLOWER!!!
    (iter (for digitff :initially digits :then (cdr digitff))
      (until (< (length digitff) 13))
      (maximize (apply #'* (subseq digitff 0 13))))))
 
;; TODO for p9:
;; 3. prevent: a = 2, b = 3 -> a = 3, b = 2 being 2 iterations.
;; in other words, avoid duplicate calculation of (c^2)
;; DONE:
;; 1. Fix ugly let
;; 2. make neater
(defun p9 ()
  (iter outer (for a from 1 to 1000)
    (iter (for b from 1 to 1000)
          (for c = (sqrt (+ (sqr a) (sqr b))))
          (when (= (+ a b c) 1000)
            (format t "~a^2 + ~a^2 = ~a^2~%" a b c)
            (return-from outer (* a b c))))))

;; Sieve of Erotosthenes Algorithm
(defun soe-primes (n)
  (let ((marked '()))
    (iter (for x from 2 to (1- n))
          (when (member x marked)
            (next-iteration))
          (iter (for y from (sqr x) below n)
            (when (zerop (mod y x))
              (push y marked)))
          (collect x))))

;;; What to do about error handling?
;; (when (or (not (integerp n))
;;           (< n 0))
;;   (error "(! n): n is either not an integer or less than 0"))
;; for choose (or (< k 0) (> k n))
(defun ! (n)
  (if (zerop n)
    1
    (* n (! (1- n)))))

(defun choose (n k)
  (/ (! n) (* (! k) (! (- n k)))))

;; Pascal's triangle
(defun binomial-coefficients (n)
  (iter (for k from 0 to n)
        (collect (choose n k))))    

;; Just for fun
(defun totative-table ()
  (format t " ~1a |" "+")
  (iter (for a from 1 to 10)
        (format t "~2a " a))
  (format t "~%")

  (format t "---+")
  (iter (for x from 1 to 10)
        (format t "---"))
  (format t "~%")

  (iter (for i from 0 to 90 by 10)
        (format t "~2a |" i)
        (iter (for j from 1 to 10)
          (format t "~2a "
            (totatives (+ i j))))
        (format t "~%")))

(defconstant PHI (* 2 (cos (/ PI 5))))

(defun coprime (a b)
  (= (gcd a b) 1))

;; From Euler's Totient Function
;; Gives a list of coprimes of n
(defun totatives (n)
  (when (= n 1) (return-from totatives 1))
  (iter (for i from 1 to (1- n))
    (when (coprime n i)
      (count i))))

;; todo: look at "to" in iter
(defun perfect-power (n)
  (iter outer (for m from 1 to n)
    (iter (for k from 2 to n)
          (when (= (expt m k) n)
            (return-from outer (values k m))))))

(defun perfect-power2 (n)
  (iter (for k from 2 to (log n 2))
        (let ((a (expt n (/ 1 k))))
          ;; If a is an integer return k and a
          (when (zerop (mod a 1)) 
            (return (values k a))))))

(defun ord-mod (n a)
  (when (coprime n a)
    (iter (for k from 1)
          (when (= (mod (expt a k) n) 1)
            (return k)))))

(defun aks-prime? (n)
  (when (perfect-power2 n)
    (return-from aks-prime? nil))

  (let* ((maxk (floor (sqr (log n 2))))
         (maxr (max 3 (ceiling (expt (log n 2) 5))))
         (nextR t)
         (my-r (iter (for r from 2 below maxr)
                 (while nextR)
                 (setf nextR nil)
                 (iter (for k from 1 to maxk)
                   (while (not nextR))
                   (setf nextR
                     (or (= (mod (expt n k) r) 1)
                         (zerop (mod (expt n k) r)))))
                 (finally (return r)))))

    (iter (for a from my-r downto 1)
          (while (> a 1))
          (let ((my-gcd (gcd a n)))
            (when (and (> my-gcd 1) (< my-gcd n))
              (return-from aks-prime? nil))))

    (when (and (<= n 5690034) (<= n my-r))
      (return-from aks-prime? t))

    (iter (for a from 1 to
            (floor (* (sqrt (totatives my-r))
                      (log n 2))))

          (when ()))))

;; (iter (for r from 2)
;;       (when (not (coprime r n))
;;         (next-iteration))
;;       (when (> (ord-mod r n) (sqr (log n 2)))
;;         (return r)))

;; (iter (for a from 2 to (min r (1- n)))
;;       (if (zerop (mod a n))
;;         (return nil)))

(defun aks-prime2? (n)
  (if (perfect-power n)
    nil
    (iter (for r from 2)
      (when (not (coprime r n)) (next-iteration))
      (when (> (ord-mod r n) (sqr (log n 2)))
        (iter (for a from 2 to (min r (1- n)))
          (if (zerop (mod a n))
            (return nil)
            (return (values a n))))))))

;; (defun mulitplicative-formula (n k)
;;   (iter (for )))

;; Miller-Rabin Primality Test
;; 2^m * d + 1
;; a^d (mod n) = +1 or -1
;; (defun miller-rabin-p (n)
;;   ;; Keep dividing n by 2 until it isn't an integer
;;   (if (and (> n 3) (oddp n))
;;     (let* ((md-form
;;              (iter (for m from 1)
;;                    (while (integerp (/ d 2)))
;;                    (with d = (/ (1- n) 2))
;;                    (minimizing (setf d (/ d 2)))
;;                    (finally (return (list m d)))))

;;             (a (1+ (random (1- n))))

;;             (2-expt
;;               (iter (repeat (cadr md-form))
;;                     (with result = 1)
;;                     (setf result (mod (* result a) n))
;;                     (finally (return result)))))

;;       (if (= (- 2-expt n) 1)
;;         (list t md-form a 2-expt)
;;         (list nil md-form a 2-expt)))

;;     t))

;; (defun miller-rabin-p2 (n &optional (tests 5))
;;   (iter (for i from 1 to tests)
;;     (if (miller-rabin-p n)
;;       (next-iteration)
;;       (return-from miller-rabin-p2 nil)))

;;   t)

;; STILL NEEDS A LITTLE FIXING
(defun miller-rabin? (n) ;&optional (tests 5)
  (when (evenp n)
    (return-from miller-rabin? nil))
  
  (let ((a 2)
        (r 0)
        (d (/ (1- n) 2))
        (x 1))
    ;; (iter outer (repeat tests)
    (iter outer (for a in '(2 3))

      ;; (setf a (+ 1 (random (1- n))))
      (setf r 0)
      (setf d (/ (1- n) 2))
      (setf x 1)

      ;; Set variables for: 2^r * d + 1
      (iter (for i from 1)
        (while (integerp (/ d 2)))
        (minimizing (setf d (/ d 2)))
        (finally (setf r i)))

      ;; Calculate x in `a^d = x (mod n)` efficiently
      ;; difference of 3.65s vs 0.055s OR 2s vs 14s
      ;; (setf x (mod (expt a d) n))
      (iter (repeat d)
            (setf x (mod (* x a) n)))

      (when (or (= x 1) (= x (1- n)))
        (next-iteration))

      (iter (repeat (1- r)) ;(for i from 0 to r)
        (setf x (mod (* x x) n)))

      ;; (setf x (mod (expt a (* (expt 2 r) d)) n))
      ;; (when (or (= x -1) (= x 1) (= x (1- n)))
      ;;   (next-iteration))

      (when (or (= x (1- n)) (= x 1))
        (next-iteration))

      (return-from miller-rabin? nil))

    (return-from miller-rabin? (list t a))))

(defun miller-rabin-g (n)
  (append '(2) (g:collect (g:filter! #'miller-rabin?
                             (g:range :from 3 :to n)))))

;;-------------------------------------------------------

(defun p11 ()
  (let ((num
'((08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08)
  (49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00)
  (81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65)
  (52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91)
  (22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80)
  (24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50)
  (32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70)
  (67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21)
  (24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72)
  (21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95)
  (78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92)
  (16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57)
  (86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58)
  (19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40)
  (04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66)
  (88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69)
  (04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36)
  (20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16)
  (20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54)
  (01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48))))

    ;; Compute diagnols going like \\\\
    ;; (iter (for i from 20 downto 0)
    ;;       (iter (for x in num)
    ;;             (for y from i)
    ;;             (while (< y (length x)))
    ;;             (format t "~a " (elt x y)))
    ;;       (format t "~%"))

    ;; (iter (for j from 0 to 20)
    ;;       (iter (for x in (nthcdr j num))
    ;;             (for y from 0)
    ;;             (while (< y (length x)))
    ;;             (format t "~a " (elt x y)))
    ;;       (format t "~%"))

    ;; Compute diagnols going like ////
    (iter (for i from 0 to 20)
          (iter (for x in num)
                (for y from i)
                (while (< y (length x)))
                (format t "~a " (elt x y)))
          (format t "~%"))
    
    ))

