(defpackage euler-cl/tests/main
  (:use :cl
        :euler-cl
        :rove))
(in-package :euler-cl/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :euler-cl)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
