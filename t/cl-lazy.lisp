(in-package :cl-user)
(defpackage cl-lazy-test
  (:use :cl
        :cl-lazy
        :prove))
(in-package :cl-lazy-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-lazy)' in your Lisp.

(plan 5)

(subtest
    "Test if it is evaluated only once"
  (let ((x (lazy (princ "first") (+ 100 200))))
    (is-print (force x) "first")
    (is-print (force x) "")))

(subtest
    "Test if output is immutable"
 (let ((x (lazy (print 'first) (+ 100 200))))
   (is (force x) 300)
   (is (force x) 300)))

(subtest
    "Test lcons, lcar & lcdr"
  (subtest
      "Test in 2 elements"
    (let ((lst (lcons (progn (princ "a") 1)
		      (lcons (progn (princ "b") 2) nil)))
	  (res nil))
      (is-print (setf res (lcar lst)) "a")
      (is res 1)
      (is-print (setf res (lcar (lcdr lst))) "b")
      (is res 2))))

(subtest
    "Test llist, lnth"
  (let ((lst (llist 1 (progn (princ "b") 2) 3))
	(res nil))
    (is (lnth -1 lst) 1)
    (is (lnth 0 lst) 1)
    (is-print (setf res (lnth 2 lst)) "b")
    (is res 3)
    (is-print (setf res (lnth 1 lst)) "")
    (is res 2)
    
    (ok (null (lnth 3 lst)))
    (ok (null (lnth 10 lst)))))

(subtest
    "Test make-number-series"
  (labels ((test-series (l-lst test-len expected)
	     (let ((lst nil))
	       (dotimes (i test-len)
		 (setf lst (cons (lnth i l-lst) lst)))
	       (setf lst (reverse lst))
	       (is lst expected :test #'equalp))))
    (test-series (make-number-series nil (* (1+ n) 2)) 5 '(2 4 6 8 10))
    (test-series (make-number-series (0 1) (+ (lnth (- n 1) a) (lnth (- n 2) a)))
		 10
		 '(0 1 1 2 3 5 8 13 21 34))))

(finalize)
