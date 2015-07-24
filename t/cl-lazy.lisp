(in-package :cl-user)
(defpackage cl-lazy-test
  (:use :cl
        :cl-lazy
        :prove))
(in-package :cl-lazy-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-lazy)' in your Lisp.

(plan 7)

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

(defun is-series (l-lst test-len expected)
  (let ((lst nil))
    (dotimes (i test-len)
      (setf lst (cons (lnth i l-lst) lst)))
    (setf lst (reverse lst))
    (is lst expected :test #'equalp)))

(subtest
    "Test make-series"
  (is-series (make-series nil (* (1+ .n) 2)) 5 '(2 4 6 8 10))
  (is-series (make-series (0 1) (+ (lnth (- .n 1) .a) (lnth (- .n 2) .a)))
	     10
	     '(0 1 1 2 3 5 8 13 21 34)))

(defparameter *old-table* (lexport-readtable))
(subtest
    "Test lexport-readtable"
  (is-expand #{a (+ n 1)} (lnth (+ n 1) a))
  (is-expand #[1 + 2] (+ 1 2))
  
  (is-series (make-series (0 1) (+ #{.a #[.n - 1]} #{.a #[.n - 2]}))
	     10
	     '(0 1 1 2 3 5 8 13 21 34))
  (subtest
      "Test unexporting readtable"
    (setf *readtable* *old-table*)
    (ok (not (get-dispatch-macro-character #\# #\{)))))

(subtest
    "Test concat-series"
  (let ((an (make-series nil (* .n 2)))
	(bn (make-series nil (* .n 3)))
	(cn (make-series nil (* .n 5))))
    (is-series (concat-series #'(lambda (a b c) (list a b c)) an bn cn)
	       4
	       '((0 0 0 ) (2 3 5) (4 6 10) (6 9 15)))))

(finalize)
