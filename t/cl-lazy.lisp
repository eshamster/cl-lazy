(in-package :cl-user)
(defpackage cl-lazy-test
  (:use :cl
        :cl-lazy
        :prove))
(in-package :cl-lazy-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-lazy)' in your Lisp.

(plan 10)

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
    "Test llist, llist-to-list"
  (let ((llst (llist 1 (progn (princ "b") 2) 3))
	(res nil))
    (is-print (setf res (llist-to-list llst)) "b")
    (is res '(1 2 3) :test #'equalp)
    (is-print (setf res (llist-to-list llst)) "")
    (is res '(1 2 3) :test #'equalp)
    
    (is (llist-to-list llst :max-length -1) '(1 2 3))
    (is (llist-to-list llst :max-length  0) nil)
    (is (llist-to-list llst :max-length  1) '(1))
    (is (llist-to-list llst :max-length  2) '(1 2))
    (is (llist-to-list llst :max-length  3) '(1 2 3))
    (is (llist-to-list llst :max-length  100) '(1 2 3))))

(subtest
    "Test do-llist"
  (let ((llst (llist 1 2 nil nil 5 nil)))
    (is-print (do-llist (val llst) (format t "~A " val))
	      "1 2 NIL NIL 5 NIL ")))

(subtest
    "Test lnth, lnthcdr"
  (let ((llst (llist 1 2 3)))
    (is (lnth -1 llst) 1)
    (is (lnth 0 llst) 1)
    
    (ok (null (lnth 3 llst)))
    (ok (null (lnth 10 llst)))))

(defun is-series (l-lst test-len expected)
  (let ((lst nil))
    (dotimes (i test-len)
      (setf lst (cons (lnth i l-lst) lst)))
    (setf lst (reverse lst))
    (is lst expected :test #'equalp)))

(subtest
    "Test make-series"
  (is-series (make-series nil #'(lambda (a n) (declare (ignore a)) (* (1+ n) 2))) 5 '(2 4 6 8 10))
  (is-series (make-series '(0 1) #'(lambda (a n) (+ (lnth (- n 1) a) (lnth (- n 2) a))))
	     10
	     '(0 1 1 2 3 5 8 13 21 34)))

(defparameter *old-table* (lexport-readtable))
(subtest
    "Test lexport-readtable"
  (is-expand #{a (+ n 1)} (lnth (+ n 1) a))
  (is-expand #[1] 1)
  (is-expand #[1*2] (* 1 2))
  (is-expand #[1+2] (+ 1 2))
  (is-expand #[1/2] (/ 1 2))
  (is-expand #[1 - 2] (- 1 2))
  (is-expand #[1 * 2] (* 1 2))
  
  (is-expand #{a[n-1]} (lnth (- n 1) a))
  (is-expand #{a[n-1][m]} (lnth m (lnth (- n 1) a)))
  
  (is-series (make-series '(0 1) #'(lambda (a n) (+ #{a[n-1]} #{a[n-2]})))
	     10
	     '(0 1 1 2 3 5 8 13 21 34))

  (subtest
      "Test #<>"
    (is-expand #<a[n] = (* n 2)>
	       (make-series nil #'(lambda (a n) (declare (ignorable a n)) (* n 2))))
    (is-expand #<a[n] = 0, 1, (+ (* a[n-1] 2) a[n-2])>
	       (make-series (list 0 1) #'(lambda (a n)
				       (declare (ignorable a n))
				       (+ (* (lnth (- n 1) a) 2) (lnth (- n 2) a)))))
    (is-series #<b[k] = 0, 1, (+ b[k-1] b[k-2])>
	       10
	       '(0 1 1 2 3 5 8 13 21 34))
    (subtest
	"Test the format of x[y] can be used in initial values"
      (let ((x #<a[n] = (* (1+ n) 2)>))
	(is-series #<b[k] = x[0], (* b[k-1] 2)>
		   5
		   '(2 4 8 16 32))))))


(lexport-readtable)
(unwind-protect
     (progn 
       (subtest
	   "Test do-series"
	 (let ((series1 #<a[n] = (* n 2)>)
	       (series2 #<a[n] = 0, nil, nil, (* n 2)>))
	   (is-print (do-series (val series1 to 5) (format t "~D " val))
		     "0 2 4 6 8 10 ")
	   (is-print (do-series (val series2 to 5) (format t "~D " val))
		     "0 NIL NIL 6 8 10 ")))

       (subtest
	   "Test concat-series"
	 (let ((an #<a[n] = (* n 2)>)
	       (bn #<a[n] = (* n 3)>)
	       (cn #<a[n] = (* n 5)>))
	   (is-series (concat-series #'(lambda (a b c) (list a b c)) an bn cn)
		      4
		      '((0 0 0 ) (2 3 5) (4 6 10) (6 9 15))))))
  
  (setf *readtable* *old-table*))

(finalize)
