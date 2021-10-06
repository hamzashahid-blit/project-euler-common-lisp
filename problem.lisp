;; These problems were taken from: https://projecteuler.net
;; Account username: hamzashahid
;; Password is in keepassxc

(defun p1 (n)
  (loop :for x :below n
    :if (or (= (mod x 3) 0)
           (= (mod x 5) 0))
    :sum x))

;;-------------------------------------------------------

;; My Fibonacci (Slowwwwwwwww)
(defun fib (n)
  (if (<= n 2)
    n
    (+ (fib (- n 1))
       (fib (- n 2)))))

;; Optimized Fibonacci (Tail recursive)
(defun fib2 (n)
  (labels ((calc-fib (n a b)
                     (if (= n 0)
                       a
                       (calc-fib (- n 1) b (+ a b)))))
    (calc-fib n 0 1)))

(defun p2 (n)
  (loop :for x :from 1
    :while (< (fib2 x) n)
    :if (evenp (fib2 x))
    :sum (fib2 x)))

;;-------------------------------------------------------

;; Is very fast but small numbers like 15 aren't accurate
;; for accurate (small nums) but slow use commented line
;;(loop :for x :from 2 :upto (1- n)
;; We can use and since it is short circuited. This is why
;; we don't have to seperate it into after math
(defun p3 (n)
  (apply #'max 
    (loop :for x :from 2 :upto (sqrt n)
      :if (and (= (mod n x) 0)
            (null (primes x)))
      collect x)))

;;-------------------------------------------------------

(defun num-palindrome-p (num)
  (let ((strnum (write-to-string num)))
    (values
      (equal strnum (reverse strnum))
      (parse-integer (reverse strnum)))))

(defun num-palindrome (num)
  (parse-integer (reverse (write-to-string num))))

(defun p4 ()
  (loop :for x :from 999 :downto 900
    :append (loop :for y :from 999 :downto 900
              :if (= (* x y)
                     (num-palindrome (* x y)))
              :collect (* x y))))

;;-------------------------------------------------------

(defun p5 ()
  (loop :for x :from 1
    :if (and (= (mod x 2) 0)
             (= (mod x 3) 0)
             (= (mod x 4) 0)
             (= (mod x 5) 0)
             (= (mod x 6) 0)
             (= (mod x 7) 0)
             (= (mod x 8) 0)
             (= (mod x 9) 0)
             (= (mod x 10) 0))
    :do (return x)))

(defmacro apply-and (n)
  `(and ,@(tmpp n)))

(defun tmpp (n)
  (mapcar
	#'(lambda (y)
        (= (mod n y) 0))
    (loop :for i :from 2 :upto 10
      :collect i)))
