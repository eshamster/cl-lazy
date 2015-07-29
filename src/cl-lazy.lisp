(in-package :cl-user)
(defpackage cl-lazy
  (:use :cl)
  (:export #:.a #:.n))
(in-package :cl-lazy)

(cl-annot:enable-annot-syntax)

@export
(defmacro lazy (&body body)
  (let ((value (gensym))
	(evaluated (gensym)))
    `(let ((,value nil)
	   (,evaluated nil))
       (lambda ()
	 (unless ,evaluated
	   (setf ,value (progn ,@body))
	   (setf ,evaluated t))
	 ,value))))

@export
(defun force (lazy-value)
  (funcall lazy-value))


@export
(defmacro lcons (a b)
  `(lazy (cons ,a ,b)))

@export
(defmacro lcar (l-lst)
  `(if (null ,l-lst)
       nil
       (car (force ,l-lst))))

@export
(defmacro lcdr (l-lst)
  `(if (null ,l-lst)
       nil
      (cdr (force ,l-lst))))

@export
(defmacro llist (&rest rest)
  `,(llist-body rest))

; (llist-body '(1 2 3) :tail tail) -> (LCONS 1 (LCONS 2 (LCONS 3 tail)))
(defun llist-body (lst &key (tail nil))
  (labels ((f (llst rest-arg)
	     (if (null rest-arg)
		 llst
		 (f (cons 'lcons (list (car rest-arg) llst)) (cdr rest-arg)))))
    (f tail (reverse lst))))


@export
(defun lnth (n l-lst)
  (if (<= n 0)
      (lcar l-lst)
      (lnth (1- n) (lcdr l-lst))))


#|
Ex1. Series of even numbers -> [0, 2, 4, 6, ...]
  (make-series nil (* .n 2))

Ex2. Fibonacci series -> [0, 1, 1, 2, 3, 5, 8, 13, ...]
  (make-series (0 1) (+ (lnth (- .n 1) .a) (lnth (- .n 2) .a)))
|#
@export
(defmacro make-series (init-nums &body body)
  (let ((f (gensym)))
    `(let ((.a nil))
       (declare (ignorable .a))
       (labels ((,f (.n)
		  (declare (ignorable .n))
		  (lcons (progn ,@body) (,f (1+ .n)))))
	 (setf .a (llist-with-tail (,f ,(length init-nums)) ,@init-nums))))))

@export
(defun make-series-fn (init-nums fn-calc)
  (let ((a nil))
    (labels ((f (n) (lcons (funcall fn-calc a n) (f (1+ n))))
	     (recursive-cons (lst tail)
	       (if (null lst)
		   tail
		   (lcons (car lst) (recursive-cons (cdr lst) tail)))))
      (setf a (recursive-cons init-nums (f (length init-nums)))))))

(defmacro llist-with-tail (tail &rest rest)
  `,(llist-body rest :tail tail))

#|
Utils
|#

@export
(defmacro concat-series (fn-concat &rest some-series)
  `(make-series nil
     (funcall ,fn-concat
	      ,@(mapcar #'(lambda (series)
			    `(lnth .n ,series))
			some-series))))

#|
Reader Macro
|#
@export
(defun lexport-readtable ()
  (let ((old-table (copy-readtable *readtable*)))
    (setf *readtable* (add-readtable))
    old-table))

(defun add-readtable ()
  (let ((*readtable* (copy-readtable *readtable*)))
    ; #{a n} -> (lnth n a)
    (set-macro-character #\} (get-macro-character #\)))
    (set-dispatch-macro-character
     #\# #\{
     #'(lambda (stream &rest rest)
	 (declare (ignore rest))
	 (let ((*readtable* (copy-readtable *readtable*))
		(pair nil))
	   (set-macro-character #\[ #'[-reader)
	   (setf pair (read-delimited-list #\} stream t))
	   `(lnth ,(cadr pair) ,(car pair)))))

    (set-dispatch-macro-character #\# #\[ #'[-reader)
    *readtable*))

(defun [-reader (stream &rest rest)
  (declare (ignore rest))
  (let ((lst nil)
	(*readtable* (copy-readtable *readtable*)))
    (set-separate-character #\-)
    (set-separate-character #\*)
    (set-macro-character #\] (get-macro-character #\)))
    (setf lst (read-delimited-list #\] stream t))
    (case (length lst)
      (1 (car lst))
      (3 (case (cadr lst)
	   (#\- `(- ,(car lst) ,(caddr lst)))
	   (#\* `(* ,(car lst) ,(caddr lst)))))
      (t (error 'simple-error)))))
  
(defun set-separate-character (char)
  (set-macro-character char
		       #'(lambda (s c)
			   (declare (ignore s c))
			   char)))
