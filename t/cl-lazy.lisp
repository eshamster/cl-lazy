(in-package :cl-user)
(defpackage cl-lazy-test
  (:use :cl
        :cl-lazy
        :prove))
(in-package :cl-lazy-test)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-lazy)' in your Lisp.

(enable-series-processing-syntax)

(plan 15)

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
    (is (llist-to-list llst :max-length  100) '(1 2 3)))

  (is (llist-to-list (llist 1 2 nil 3)) '(1 2))
  (is (llist-to-list (llist 1 2 nil 3) :stops-at-nil nil) '(1 2 nil 3)))

(subtest
    "Test lreverse"
  (is (llist-to-list (lreverse (llist 1 2 3)))
      '(3 2 1)
      :test #'equal))

(subtest
    "Test lspan"
  (is (multiple-value-bind (target rest)
          (lspan (lambda (x) (< x 3)) (llist 1 2 3 2 1))
        (list (llist-to-list target)
              (llist-to-list rest)))
      '((1 2) (3 2 1))
      :test #'equal)
  (is (multiple-value-bind (target rest)
          (lspan (lambda (x) (< x 0)) (llist 1 2 3 2 1))
        (list (llist-to-list target)
              (llist-to-list rest)))
      '(() (1 2 3 2 1))
      :test #'equal)
  (is (multiple-value-bind (target rest)
          (lspan (lambda (x) (< x 100)) (llist 1 2 3 2 1))
        (list (llist-to-list target)
              (llist-to-list rest)))
      '((1 2 3 2 1) ())
      :test #'equal))

(subtest
    "Test lappend"
  (let ((llst (llist 1 2)))
    (is (llist-to-list (lappend llst (llist 3 4)))
        '(1 2 3 4)
        :test #'equal)
    (is (llist-to-list llst) '(1 2) :test #'equal))
  (is (llist-to-list (lappend (llist 1 2) (llist 3 4) (llist 5 6)))
      '(1 2 3 4 5 6)
      :test #'equal)
  (let ((series #<a[n] = (* n 2)>))
    (is (llist-to-list (lappend (llist 100 200) series)
                       :max-length 4)
        '(100 200 0 2)
        :test #'equal)))

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

(defparameter *old-table* (enable-series-processing-syntax))
(subtest
    "Test enable-series-processing-syntax"
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

(unwind-protect
     (progn
       (subtest
	   "Test is-series-end"
	 (let ((s #<a[n] = 0, 1, nil>))
	   (ok (not (is-series-end s)))
	   (ok (not (is-series-end (lnthcdr 1 s))))
	   (ok (is-series-end (lnthcdr 2 s)))
	   (ok (is-series-end (lnthcdr 3 s)))))
       
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
		      '((0 0 0 ) (2 3 5) (4 6 10) (6 9 15)))))
       (subtest
	   "Test filter-seires & filter-series-with-little"
	 (let ((a #<a [n] = n>))
	   (is-series (filter-series #'oddp a)
		      5
		      '(1 3 5 7 9))
	   (is-series (filter-series
		       #'(lambda (val)
			   (= (mod val 100) 20))
		       a
		       :give-up-distance 50)
		      3
		      '(20 nil nil))
	   (is-series (filter-series-using-little
		       #'(lambda (val a n)
			   (if (= n 0)
			       (= val 2)
			       (= (mod val #{a[n-1]}) 0)))
		       a)
		      5
		      '(2 4 8 16 32)))))
  
  (setf *readtable* *old-table*))

(finalize)
