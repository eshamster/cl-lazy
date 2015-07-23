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
       (labels ((,f (.n) (lcons (progn ,@body) (,f (1+ .n)))))
	 (setf .a (llist-with-tail (,f ,(length init-nums)) ,@init-nums))))))

(defmacro llist-with-tail (tail &rest rest)
  `,(llist-body rest :tail tail))

@export
(defun lexport-readtable ()
  (let ((old-table (copy-readtable *readtable*)))
    (setf *readtable* (add-readtable))
    old-table))

(defun add-readtable ()
  (let ((table (copy-readtable *readtable*)))
    ; #{a n} -> (lnth n a)
    (set-macro-character #\} (get-macro-character #\)) nil table)
    (set-dispatch-macro-character
     #\# #\{
     #'(lambda (stream &rest rest)
	 (declare (ignore rest))
	 (let ((pair (read-delimited-list #\} stream t)))
	   `(lnth ,(cadr pair) ,(car pair))))
     table)

    ; #[a b c] -> (b a c)
    ; Ex. #[n + 1] -> (+ n 1)
    (set-macro-character #\] (get-macro-character #\)) nil table)
    (set-dispatch-macro-character
     #\# #\[
     #'(lambda (stream &rest rest)
	 (declare (ignore rest))
	 (let ((tri (read-delimited-list #\] stream t)))
	   `(,(cadr tri) ,(car tri) ,(caddr tri))))
     table)
    table))

