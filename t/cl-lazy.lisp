(in-package :cl-user)
(defpackage cl-lazy-test
  (:use :cl
        :cl-lazy
        :prove))
(in-package :cl-lazy-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-lazy)' in your Lisp.

(plan nil)

#|
(prove:subtest
    "Test lazy evaluation" 
  (prove:subtest
     "Test if it is evaluated only once"
    (let ((x (lazy (princ "first") (+ 100 200))))
      (prove:is-print (force x) "first")
      (prove:is-print (force x) "")))
 
  (prove:subtest
      "Test if output is immutable"
    (let ((x (lazy (print 'first) (+ 100 200))))
      (prove:is (force x) 300)
      (prove:is (force x) 300)))
  
  (prove:subtest
      "Test lazy-car lazy-cdr"
    (let ((x (lazy '(1 2 3))))
      (prove:is (lazy-car x) 1)
      (prove:is (lazy-cdr x) '(2 3))))
  
  (prove:subtest
      "Test lazy-setf-cdr"
    (let ((x (lazy '(1 2 3))))
      (prove:is (lazy-setf-cdr x) '(2 3))
      (prove:is (force x) '(2 3)))))
|#

(finalize)
