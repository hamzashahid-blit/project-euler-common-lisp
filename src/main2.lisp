;;;; All main comments common to both files are in main.lisp

;;;; "-n" suffix is for "new" functions using libraries

(defpackage euler-cl
  (:use :cl :iterate) ; :gtwiwtg :cl-punch :str 
  (:local-nicknames (:a :alexandria)
                    (:g :gtwiwtg)))
;; (:shadowing-import-from :iterate)

(in-package :euler-cl)

(cl-punch:enable-punch-syntax)

(defun rel-path (path)
  (asdf:system-relative-pathname :euler-cl path))

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

    (iter (for (a b c d e f g h i j k l m) on digits)
      (while m)
      (collect (* a b c d e f g h i j k l m)))))

    ;; SLOWER!!!
    ;; (iter (for digitff :initially digits :then (cdr digitff))
    ;;   (until (< (length digitff) 13)) ;;   (maximize (apply #'* (subseq digitff 0 13))))))
 
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

;; Sieve of Sundaram Algorithm
(defun sos-primes (num)
  (let* ((k (- num 2))
         (primes (make-array num
                   :fill-pointer 0
                   :element-type 'fixnum
                   :adjustable t)))

    (iter (for i from 1 to num)
          (vector-push-extend i primes))

    (iter (for i from 1 to (1+ k))
          (for j = i)
          (iter (until (<= (+ i j (* 2 i j)) k))
            (setf primes (remove-if (constantly t) primes
                           :start (+ i j (* 2 i j)) :count 1))
            (incf j)))
    
    (when (> num 2)
      (format t "2 "))

    (iter (for i from 1 to (1+ k))
      (format t "~a " (* 2 (1+ i))))

    (reduce #'+ primes)))

;; Sieve of Erotosthenes Algorithm
(defun soe-primes2 (num)
  ;; Make this a vector to optimize
  (let ((primes
          (iter (for i from 3 to num by 2)
                (collect i))))

    (iter outer (for i in primes)
                (for counter from 1)
          (iter (for x in (nthcdr counter primes))
                (when (> (sqr i) num)
                  (return-from outer))
                (when (zerop (mod x i))
                  (a:deletef primes x)
                  ;; (setf primes (delete x primes :test #'=))
                  )))

    (apply #'+ (cons 2 primes))))

;; ;; Sieve of Erotosthenes Algorithm
(defun soe-primes3 (n)
  (let ((marked '()))
    (iter (for x from 2 to (1- n))
          (when (member x marked)
            (next-iteration))
          (iter (for y from (sqr x) below n)
            (when (zerop (mod y x))
              (push y marked)))
          (sum x))))

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

;;; YOOOOOOOOOOOOOOOOOOO IT FINALLY WORKED
(defun beach-primes (n)
  (let ((result (make-array 1000 :adjustable t :fill-pointer 2)))
    (setf (aref result 0) 2
          (aref result 1) 3)
    (loop :for candidate :from 5 :to n :by 2
          :do (loop :for prime-index :from 1
                    :for prime = (aref result prime-index)
                    :until (> (* prime prime) candidate)
                    ;; :while (<= prime (sqrt candidate))
                    :when (zerop (mod candidate prime))
                      :do (return)
                    :finally (vector-push-extend candidate result)))
    ;(reduce #'+ result)
    ))

(defun sum-of-primes (n)
  (reduce #'+ (beach-primes n)))


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
  (01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48)))
         (row-prods '())
         (col-prods '())
         (upper-diag-prods-lr '())
         (lower-diag-prods-lr '())
         (upper-diag-prods-rl '())
         (lower-diag-prods-rl '())
         (row-prod nil)
         (col-prod nil)
         (upper-diag-prod-lr nil)
         (lower-diag-prod-lr nil)
         (upper-diag-prod-rl nil)
         (lower-diag-prod-rl nil))

    ;; Lower half
    (format t "~&~%+----- Lower DIAGS Right-Left -----+~%~%") 
    (iter (for i from 1 to 20)
          (for list = '())
          (iter (for y in (nthcdr i num))
                (for x from 19 downto 0)
                (push (elt y x) list))
          (iter (for (a b c d) on list)
                (while d)
                (format t "~a * ~a * ~a * ~a = ~a~%"
                  a b c d (car (push (* a b c d)
                                     lower-diag-prods-rl))))
          (format t "~&------------------~%")
          (finally (format t "FINAL LOWER DIAG PRODUCT: ~a~%"
                             (setf lower-diag-prod-rl
                                   (reduce #'max lower-diag-prods-rl)))))

    ;; Compute Diagnols RL like ///
    ;; Upper Half
    (format t "~&~%+----- Upper DIAGS Right-Left -----+~%~%") 
    (iter (for i from 0 to 19)
          (for list = '())
          (iter (for y in num)
                (for x from i downto 0)
                (while (and (>= x 0) (<= x 20)))
                (push (elt y x) list))
          (iter (for (a b c d) on list)
            (while d)
            (format t "~a * ~a * ~a * ~a = ~a~%"
                      a b c d (car (push (* a b c d)
                                         upper-diag-prods-rl))))
          (format t "~&------------------~%")
          (finally (format t "FINAL UPPER DIAG RL PRODUCT: ~a~%"
                             (setf upper-diag-prod-rl
                                   (reduce #'max upper-diag-prods-rl)))))

    ;; Compute Diagnols LR like \\\
    ;; Upper Half
    (format t "~&~%+----- Upper DIAGS -----+~%~%") 
    (iter (for i from 20 downto 0)
          (for list = '())
          (iter (for x in num)
                (for y from i)
                (while (< y (length x)))
                (push (elt x y) list))
          (iter (for (a b c d) on list)
            (while d)
            (format t "~a * ~a * ~a * ~a = ~a~%"
                      a b c d (car (push (* a b c d)
                                         upper-diag-prods-lr))))
          (format t "~&------------------~%")
          (finally (format t "FINAL UPPER DIAG PRODUCT: ~a~%"
                             (setf upper-diag-prod-lr
                                   (reduce #'max upper-diag-prods-lr)))))

    ;; Lower half
    (format t "~&~%+----- Lower DIAGS -----+~%~%") 
    (iter (for i from 1 to 20)
          (for list = '())
          (iter (for x in (nthcdr i num))
                (for y from 0 to 20)
                (push (elt x y) list))
          (iter (for (a b c d) on list)
                (while d)
                (format t "~a * ~a * ~a * ~a = ~a~%"
                  a b c d (car (push (* a b c d)
                                     lower-diag-prods-lr))))
          (format t "~&------------------~%")
          (finally (format t "FINAL LOWER DIAG PRODUCT: ~a~%"
                             (setf lower-diag-prod-lr
                                   (reduce #'max lower-diag-prods-lr)))))

    ;; Computer Rows like ===
    (format t "~&~%+----- ROWS -----+~%~%") 
    (iter (for y in num)
          (iter (for (a b c d) on y)
                (while d)
                (format t "~a * ~a * ~a * ~a = ~a~%"
                          a b c d (car (push (* a b c d) row-prods))))
          (format t "~&------------------~%")
          (finally (format t "FINAL ROW PRODUCT: ~a"
                     (setf row-prod (reduce #'max row-prods)))))

    ;; Compute Columns like ||||
    (format t "~&~%+----- COLS -----+~%~%") 
    (iter (for x from 0 to 19)
          (iter (for i from 0 to (* 4 4))
                (for list = '())
                (iter (for c from 0 to 3)
                      (push (elt (elt num (+ i c)) x) list))
                (format t "~{~a * ~a * ~a * ~a~} = ~a~%"
                          list (car (push (apply #'* list) col-prods))))
          (format t "~&------------------~%")
          (finally (format t "FINAL COL PRODUCT: ~a"
                     (setf col-prod (reduce #'max col-prods)))))

    (format t "~&~%+------- ANSWER -------+~%") 
    (format t "ANSWER for Problem 11: ~a"
              (max row-prod col-prod
                   lower-diag-prod-rl upper-diag-prod-rl
                   lower-diag-prod-lr upper-diag-prod-lr))
    (format t "~&+----------------------+~%")))

;;-------------------------------------------------------

;; (defun divisors (n)
;;   (let ((divs (iter (for i from 2 below n)
;;                     (when (zerop (mod n i))
;;                       (collect i)))))
;;     (a:flatten (list 1 divs n))))

;; (defun divisors-length (n)
;;   (let ((fst-div (iter (for i from 2 below n)
;;                        (when (zerop (mod n i))
;;                          (return i))))
;;         (c 2))

;;     (if (null fst-div)
;;       c
;;       (iter (for i from fst-div to (/ n fst-div))
;;             (when (zerop (mod n i))
;;               (incf c))
;;             (finally (return c))))))

;; (defun prime-factors (n)
;;   )

;; (defun triangle-number (n)
;;   (/ (* n (1+ n)) 2))

;; (defun p12 (num &optional (max-primes 10000000))
;;   (format t "Generating primes upto ~a~%" max-primes)
;;   (let (;(primes (coerce (fastest-primes max-primes) 'list))
;;          )
;;     (format t "Done~%")
;;     (iter (for i from 2000000 by 2)
;;           ;; when (and (zerop (mod i 10))
;;           ;;        (not (member i primes)))
;;       (for tri = (triangle-number i))
;;       (when (> (divisors-length tri) num)
;;         (return tri)))))

;; (defun triangle-number-slow (n)
;;   (iter (for x from 1 to n)
;;         (sum x)))

;; SLOOOOW;Fast: (n(n+1)) / 2
;; (defun p12-slow (num)
;;   (iter (for i from 1)
;;         (for tri = (triangle-number-slow i))
;;         (when (> (+ 2 (length (divisors tri))) num)
;;           (return tri))))










(defun p10 ()
  (let* ((initial-primes #(2 3 5))
         (sum (reduce #'+ initial-primes :initial-value 0))
         (primes (make-array (length initial-primes)
                             :element-type '(unsigned-byte 32)
                             :initial-contents initial-primes
                             :adjustable t
                             :fill-pointer t)))
    ;; (declare (dynamic-extent primes)) Slower
    (loop :named outer-loop
          :for candidate :from 7 :below 2000000 :by 2
          :do (loop :named inner-loop
                    :for prime :across primes
                    :until (> (* prime prime) candidate)
                    ;; :while (<= prime (sqrt candidate)) faster then isqrt but slow
                    :when (zerop (mod candidate prime))
                    :do (return-from inner-loop)
                    :finally (progn
                               (incf sum candidate)
                               (vector-push-extend candidate primes)))
                    :finally (let ((counter (length primes)))
                               (return-from p10 (values counter sum))))))


;;; LONG TIME LATER ATTEMPT

;; CREDIT FOR DIVISORS => https://github.com/skeeto/euler-cl/blob/master/common.lisp#L17
(defun divisors (n)
  (loop :for i :from 1 :to (sqrt n)
        :when (zerop (mod n i)) :collect i :into small :and :collect (/ n i) :into large
        :finally (return (delete-duplicates (nconc small (nreverse large))))))

(defun p12 ()
  (loop :for x :from 1
        :for i := x :then (+ i x)
        :when (> (length (divisors i)) 500)
        :return i))

;; ;; UNOPTIMIZED
;; (defun divisor-count (n)
;;   (+ 2 (loop :for i :from 2 :to (1- n)
;;              :when (zerop (mod n i))
;;              :count i)))
;; ;; OPTIMIZED
;; (defun divisor-count (n)
;;   (let* ((limit (if (evenp n) (/ n 2) (1- n)))
;;          (result (loop :for i :from 2 :upto (sqrt (1+ n))
;;                        :when (zerop (mod n (the fixnum i)))
;;                        :count i)))
;;     (declare (type fixnum n limit)
;;              (type (unsigned-byte 32) result)
;;              (optimize (safety 0) (debug 0) (speed 3)))
;;     (+ 2 result)))
;; ;; USING PRIMES
;; (defun p12-primes (n)
;;   (let* ((initial-primes #(2 3 5))
;;          (primes (make-array (length initial-primes)
;;                              :element-type '(unsigned-byte 32)
;;                              :initial-contents initial-primes
;;                              :adjustable t
;;                              :fill-pointer t)))
;;     (loop :named outer-loop
;;           :for candidate :from 7 :below n :by 2
;;           :do (loop :named inner-loop
;;                     :for prime :across primes
;;                     :until (> (* prime prime) candidate)
;;                     :when (zerop (mod candidate prime))
;;                     :do (return-from inner-loop)
;;                     :finally (vector-push-extend candidate primes))
;;           :finally (return-from p12-primes primes))))

;; (defparameter p12-primes (reverse (coerce (p12-primes 20000000) 'list)))

;;-------------------------------------------------------

(defun p13 ()
    ;; Get text from file and remove the last empty line
    (let* ((num-strs (str:split #\Newline
                       (a:read-file-into-string
                         (rel-path "src/p13.txt"))))
            (nums-strs (subseq num-strs 0 (1- (length num-strs)))))

      ;; Loop through the list of strings, parse and sum them
      (loop :for str :in nums-strs
        :sum (parse-integer str))))

(defun collatz-seq (n)
  (if (= n 1)
    (list n)
    (if (oddp n)
      (cons n (collatz-seq (1+ (* 3 n))))
      (cons n (collatz-seq (/ n 2))))))

(defun p14 ()
  (iter (for n from 1 below 1000000)
        (finding n maximizing (length (collatz-seq n)))))

;; FASTEST But slower than (P10)
(defun fastest-primes (n)
  (let* ((initial-primes #(2 3 5))
         (sum (reduce #'+ initial-primes :initial-value 0))
         (primes (make-array (length initial-primes)
                             :element-type '(unsigned-byte 32)
                             :initial-contents initial-primes
                             :adjustable t
                             :fill-pointer t)))
    ;; (declare (dynamic-extent primes)) Slower
    (loop :named outer-loop
          :for candidate :from 7 :below n :by 2
          :do (loop :named inner-loop
                    :for prime :across primes
                    :until (> (* prime prime) candidate)
                    ;; :while (<= prime (sqrt candidate)) faster then isqrt but slow
                    :when (zerop (mod candidate prime))
                    :do (return-from inner-loop)
                    :finally (progn
                               (incf sum candidate)
                               (vector-push-extend candidate primes)))
                    :finally (return-from fastest-primes (values primes sum)))))

;; FASTEST, EVEN FASTER THAN FASTEST-PRIMES!!!
(defun p10 ()
  (let* ((initial-primes #(2 3 5))
         (sum (reduce #'+ initial-primes :initial-value 0))
         (primes (make-array (length initial-primes)
                             :element-type '(unsigned-byte 32)
                             :initial-contents initial-primes
                             :adjustable t
                             :fill-pointer t)))
    ;; (declare (dynamic-extent primes)) Slower
    (loop :named outer-loop
          :for candidate :from 7 :below 2000000 :by 2
          :do (loop :named inner-loop
                    :for prime :across primes
                    :until (> (* prime prime) candidate)
                    ;; :while (<= prime (sqrt candidate)) faster then isqrt but slow
                    :when (zerop (mod candidate prime))
                    :do (return-from inner-loop)
                    :finally (progn
                               (incf sum candidate)
                               (vector-push-extend candidate primes)))
                    :finally (let ((counter (length primes)))
                               (return-from p10 (values counter sum))))))


;; (defun p15 (m n)
;;   (labels ((calc-paths (m n x y a)
;;              (let ((acc a))
;;                (cond
;;                  ((= m 1) (1+ n))
;;                  ((= n 1) (1+ m))
;;                  (t
;;                    (calc-paths (1- m) n m n a))))))
;;     (calc-paths m n 0 0 0)))

;; My answer
(defun p15-slow (m n)
  "Lattice path of a MxN grid"
  (cond
    ((= m 1) (1+ n))
    ((= n 1) (1+ m))
    (t (+ (p15-slow m (1- n)) (p15-slow (1- m) n))))) 

;; NOT my answer
(defun p15-fastest ()
  (choose (+ 20 20) 20))

;; ---------------

(defun sum-of-digits (num)
  (iter (for x in-string (write-to-string num))
        (sum (parse-integer (string x)))))

(defun p16 (&optional (n 1000))
  (sum-of-digits (expt 2 n)))

;; ---------------

(defun p17 (&optional (n 1000))
  (loop :with acc = 0
        :for i :from 1 :to n
        :when (and (> i 100) (not (zerop (mod i 100))))
          :do (incf acc 3)
        :do (let* ((string (format nil "~r" i))
                   (cleaned-string (remove-if (lambda (char)
                                                (or (char= #\- char)
                                                    (char= #\Space char)))
                                              string))
                   (amount (length cleaned-string)))
              (incf acc amount))
        :finally (return-from p17 acc)))

;; ---------------

(defparameter p18-small '((3)
                         (7 4)
                        (2 4 6)
                       (8 5 9 3)))
(defparameter p18-big '((75)
                       (95 64)
                      (17 47 82)
                     (18 35 87 10)
                    (20 04 82 47 65)
                   (19 01 23 75 03 34)
                  (88 02 77 73 07 63 67)
                 (99 65 04 28 06 16 70 92)
                (41 41 26 56 83 40 80 70 33)
               (41 48 72 33 47 32 37 16 94 29)
              (53 71 44 65 25 43 91 52 97 51 14)
             (70 11 33 28 77 73 17 78 39 68 17 57)
            (91 71 52 38 17 14 91 43 58 50 27 29 48)
           (63 66 04 68 89 53 67 30 73 16 69 87 40 31)
          (04 62 98 27 23 09 70 98 73 93 38 53 60 04 23)))

;; if rows = 4, paths as 0 = left, 1 = right adjacent, every digit is 1 more depth
;; col = bit + prevbit 
;; 0 | 0 0 0   | 0 0 0   | 1 0, 2 0, 3 0
;; 0 | 0 0 1   | 0 0 1   | 
;; 0 | 0 1 0   | 0 1 1   | 
;; 0 | 0 1 1   | 0 1 2   | 
;; 0 | 1 0 0   | 1 1 1   | 
;; 0 | 1 0 1   | 1 1 2   | 
;; 0 | 1 1 0   | 1 2 2   | 
;; 0 | 1 1 1   | 1 2 3   | 
;; We made it to binary!!!

;; could do it as bit vector
(defun p18-binary-paths (rows)
  (loop :for j :below (expt 2 (1- rows))
        :collect (loop :for i :from (- rows 2) :downto 0
                       :collect (if (logbitp i j) 1 0))))

(defun p18-relative-paths (rows)
  (loop :for path :in (p18-binary-paths rows)
    :collect (cons 0 (let ((paths '()))
                       (reduce (lambda (a b) 
                                 (a:appendf paths (list (+ a b)))
                                 (+ a b)) 
                         path)
                       (cons (car path) paths)))))

(defun p18-paths (triangle)
  (loop :for path :in (p18-relative-paths (length triangle))
        :collect (loop :for row :in path
                       :for i :from 0
                       :collect (elt (elt triangle i) row))))

(defun p18 (triangle)
  (apply #'max (mapcar (lambda (x) (apply #'+ x)) (p18-paths triangle))))

;; <pjb> HamzaShahid: also, why use list of bits? You could use bit-vectors
;; 	  instead.  (defun integer-to-list-of-bits (n width) (loop with result =
;; 	  (make-array width :element-type 'bit :initial-element 0) for i below
;; 	  width when (logbitp i n) do (setf (aref result i) 1) finally (return
;; 	  result)))  (all-bit-combinations 3) #| --> (#*000 #*100 #*010 #*110
;; 	  #*001 #*101 #*011 #*111) |#
;; <pjb> HamzaShahid: perhaps you could even use 2D arrays of bits? (make-array
;; 	  '(8 3) :element-type 'bit :initial-contents (all-bit-combinations 3)) #|
;; 	  --> #2A(#*000 #*100 #*010 #*110 #*001 #*101 #*011 #*111) |# ?  [01:33]

;; ---------------

(defun p20 ()
  (sum-of-digits (! 100)))

;; ---------------

(defun p22 ()
  (let* ((names (with-open-file (in (rel-path "res/names.txt"))
                  (read in)))
         (sorted-names (sort names #'string<)))

    (loop :with total := 0
          :for name :in sorted-names
          :for pos :from 1
          :do (str:capitalize name)
              (let ((name-value
                      (loop :named inner
                            :with acc := 0
                            :for char :across name
                            :if (= pos 938)
                              :do (format t "~&~%~a ~a,~%~%" char (- (char-code char) 64))
                            :do (incf acc (- (char-code char) 64))
                            :finally (return-from inner acc))))
                (incf total (* pos name-value))
                (format t "~a: ~a * ~a = ~a~%" name pos name-value (* pos name-value)))
          :finally (return-from p22 total))))

;; ---------------

(defun swap (a b list)
  "Destructively swap two elements in a LIST by the indexes A & B"
  (let ((tmp))
    (setf tmp (elt list a))
    (setf (elt list a) (elt list b))
    (setf (elt list b) tmp)))

(defun p24 (&optional (list '(0 1 2)))
  (loop :named outer :for c from 0 to 10
    :do (multiple-value-bind (k l)

          ;; Generate K & L
          (loop :named inner
                :with k := -1
                :for (a b) :on list :while b
                :when (and (< a b) (> a k))
                  :do (setf k (position a list))
                :finally (if (= k -1)
                           (return-from outer)
                           (return-from inner
                               (values k (position (apply #'max list)
                                           list)))))

          ;; Swap
          (swap k l list)

          ;; Reverse
          (setf list (concatenate 'list
                       (subseq list 0 (1+ k))
                       (reverse (subseq list (1+ k)))))

          (format t "(~{~a~^ ~}) | ~a, ~a~%" list k l))))

          ;; (loop :named inner
          ;;   :with k := -1
          ;;   :with k+1 := 0
          ;;   :with flag := 0
          ;;   :for n :in list
          ;;   :for i :from 0
          ;;   :when (> n (elt list k+1))
          ;;   :do (incf flag)
          ;;   (setf k k+1)
          ;;   (setf k+1 i)
          ;;   :finally (if (= k -1)
          ;;              (return-from outer)
          ;;              (return-from inner
          ;;                (values k (position (apply #'max list)
          ;;                                    list)))))


(defun p25 ()
  (loop :for i :from 0
        :when (>= (length (write-to-string (fib2 i))) 1000)
          :return i))

;; ---------------------------------------------

(defun pandigital? (n)
  "Returns T if N is 1 through 9 pandigital, and NIL otherwise.
N must be an integer. This also means that N must also not contain any other digits
besides 1 to 9 except at the start."
  (let ((num (write-to-string n))
        (has-digits (list nil nil nil nil nil nil nil nil nil)))
    (loop :for char :across num
          :for digit := (parse-integer (string char))
          :do (unless (= digit 0)
                (if (and (>= digit 1) (<= digit 9)
                         (null (elt has-digits (1- digit))))
                  (setf (elt has-digits (1- digit)) t)
                  (return-from pandigital? nil))))
    (every #'identity has-digits)))

;; https://math.stackexchange.com/questions/195396/searching-for-pandigital-numbers
;; instead of doing a = 1..9999 & b = 1..9999
;; we know that c <= 9999 since 10 digit pandigital number
;; and a * b = c  therefore,  b = c/a
;; as for a, 9999/2 = 5000 (almost)
;; we know that 500 * 2 = 2 * 500 so we can halve
(defun p32 ()
  (let ((products '()))
    (loop :for a :from 1 :to 5000
          :do (loop :for b :from a :to (/ 9999 a)
                    :for product := (* a b) 
                    :for identity := (str:concat (write-to-string a)
                                                 (write-to-string b)
                                                 (write-to-string product))
                    :do (when (pandigital? (parse-integer identity))
                          (format t "FOUND: ~a x ~a = ~a~%" a b product)
                          (push product products))))
    (format t "Products:     ~a~%" products)
    (format t "Deduplicated: ~a~%" (remove-duplicates products :test #'=))
    (apply #'+ (remove-duplicates products :test #'=))))

;; ---------------------------------------------

(defun divisors (n)
  (let ((divs (loop :for i :from 2 :below n
                    :when (zerop (mod n i))
                      :collect i)))
    (cons 1 divs)))

(defun divisors-sum (n)
  (reduce #'+ (divisors n)))

(defun abundant-p (n)
  (> (divisors-sum n) n))

(defun sum-of-two-abundants-p (n)
  (let ((abundants
          (loop :for i :from 1 :to 28123
                :when (abundant-p i)
                :collect i)))
    (loop :for i :from 1 :below n
          :when (and (abundant-p i) (abundant-p (- n i)))
            :do (return-from sum-of-two-abundants-p t))))

(defun p23 ()
  (loop :for i :from 1 :to 28123
        :when (not (sum-of-two-abundants-p i))
          :collect i))




;; (defun abundant-numbers ()
;;   "List of abundant numbers from 12 to 28123"
;;   (let ((list '()))
;;     (loop :for n :from 12 :to 28123
;;           :do (when (> (divisors-sum n) n)
;;                 (push n list)))
;;     (reverse list)))

;; (defun sums-of-abundant-numbers ()
;;   (format t "Generating abundant numbers...~%")
;;   (let ((abundant-numbers (abundant-numbers))
;;         (list '()))
;;     (format t "Calculating sums...~%")
;;     (loop :for i :in abundant-numbers
;;           :do (loop :for j :in abundant-numbers
;;                 :do (push (+ i j) list))
;;               (print i))
;;     (format t "Done.~%")
;;     (reverse list)))

;; (defun p23 ()
;;   (let ((abundant-sums (sums-of-abundant-numbers))
;;         (list '())) 
;;     (loop :for i :from 24 :to 28123
;;           :do (unless (member i abundant-sums)
;;                 (push i list)))))

;; (defun p23 ()
;;   (set-difference 
;;     (loop :for i :from 24 :to 28123
;;       :collect i)
;;     (sums-of-abundant-numbers)))

