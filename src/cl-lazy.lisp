#|--------
  Basic
  --------|#
(in-package :cl-user)
(defpackage cl-lazy
  (:use :cl))
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
(defun llist-to-list (llist &key (max-length -1))
  (labels ((f (lst rest-llist rest-length)
	     (if (or (null rest-llist)
		     (and (>= max-length 0) (<= rest-length 0)))
		 lst
		 (f (cons (lcar rest-llist) lst)
		    (lcdr rest-llist)
		    (1- rest-length)))))
    (reverse (f nil llist max-length))))

@export
(defun lnth (n l-lst)
  (lcar (lnthcdr n l-lst)))

@export
(defun lnthcdr (n l-lst)
  (if (<= n 0)
      l-lst
      (lnthcdr (1- n) (lcdr l-lst))))

@export
(defmacro do-llist ((value llist) &body body)
  (let ((f (gensym))
	(rest (gensym)))
    `(labels ((,f (,rest)
		(if (null ,rest)
		    (return-from ,f))
		(let ((,value (lcar ,rest)))
		  ,@body
		  (,f (lcdr ,rest)))))
       (,f ,llist))))

#|
Ex1. Series of even numbers -> [0, 2, 4, 6, ...]
  (make-series nil #'(lambda (a n) (* n 2)))

Ex2. Fibonacci series -> [0, 1, 1, 2, 3, 5, 8, 13, ...]
  (make-series '(0 1) #'(lambda (a n) (+ (lnth (- n 1) a) (lnth (- n 2) a)))
|#
@export
(defun make-series (init-nums fn-calc)
  (let ((a nil))
    (labels ((f (n) (lcons (funcall fn-calc a n) (f (1+ n)))))
      (setf a (recursive-lcons init-nums (f (length init-nums)))))))

(defun recursive-lcons (lst tail)
  (if (null lst)
      tail
      (lcons (car lst) (recursive-lcons (cdr lst) tail))))

#|-------
   Utils
  -------|#

@export
(defmacro do-series ((value series to last-index) &body body)
  (unless (equal (symbol-name to) (symbol-name 'to))
    (error 'simple-error))
  (let ((counter (gensym))
	(last (gensym)))
    `(block blk
       (let ((,counter 0)
	     (,last ,last-index))
	 (do-llist (,value ,series)
	   (if (> ,counter ,last)
	       (return-from blk))
	   ,@body
	   (incf ,counter))))))

@export
(defmacro concat-series (fn-concat &rest some-series)
  (let ((a (gensym))
	(n (gensym)))
    `(make-series
      nil
      #'(lambda (,a ,n)
	  (declare (ignore ,a))
	  (funcall ,fn-concat
		   ,@(mapcar #'(lambda (series)
				 `(lnth ,n ,series))
			     some-series))))))

@export
(defun filter-series (fn-find series &key (give-up-distance 10000))
  (filter-series-using-little #'(lambda (value a n)
				   (declare (ignore a n))
				   (funcall fn-find value))
			       series
			       :give-up-distance give-up-distance))

@export
(defun filter-series-using-little (fn-find series &key (give-up-distance 10000))
  (let ((rest-series series)
	(has-give-up nil))
    (make-series
     nil
     #'(lambda (a n)
	 (block blk
	   (if has-give-up (return-from blk nil))
	   (loop for i from 0 to give-up-distance do
		(let ((target (lcar rest-series)))
		  (setf rest-series (lcdr rest-series))
		  (if (funcall fn-find target a n)
		      (return-from blk target))))
	   (setf has-give-up t)
	   nil)))))

#|-------------
  Reader Macro
  -------------|#
@export
(defun lexport-readtable ()
  (let ((old-table (copy-readtable *readtable*)))
    (setf *readtable* (add-readtable))
    old-table))

(defun add-readtable ()
  (let ((*readtable* (copy-readtable *readtable*)))
    (set-dispatch-macro-character #\# #\< #'<-reader)
    (set-dispatch-macro-character #\# #\{ #'{-reader)
    (set-dispatch-macro-character #\# #\[ #'[-reader)
    *readtable*))

; This enables to make a series as
; #<a[n] = 0, 1, (+ a[n-1] a[n-2])>
(defun <-reader (stream &rest rest)
  (declare (ignore rest))
  (let ((*readtable* (copy-readtable *readtable*))
	(buf) (a) (n))
    (set-separate-character #\>)
    (set-separate-character #\=)
    (set-separate-character #\[)
    (set-separate-character #\])
    (setf buf (read-delimited-list #\= stream t))
    ; TODO: check #\[ #\]
    (setf a (car buf))
    (setf n (caddr buf))

    (set-macro-character #\[ #'(lambda (s c)
				 (list #\[
				       (funcall #'[-reader s c))))
    (set-separate-character #\,)
    (setf buf (read-delimited-list #\> stream t))
    (labels ((sort-ref-series (buf)
	       #|
	       Basically this function traces the list recursively
	       and only reconstructs the same list.
	       But if finds (#\[ (a b)), sorts this to (lnth b a).
	       |#
	       (let ((res nil))
		 (dolist (elem buf)
		   (if (listp elem)
		       (let ((child (sort-ref-series elem)))
			 (when (eq (car child) #\[)
			   (setf child `(lnth ,(cadr child) ,(car res)))
			   (setf res (cdr res)))
			 (setf res (cons child res)))
		       (setf res (cons elem res))))
		 (reverse res))))
      (let* ((splitted (split-by-last buf #\,))
	     (init-list (remove #\, (car splitted)))
	     (body-list (cadr splitted)))
	`(make-series ,(if (null init-list) nil `(list ,@(sort-ref-series init-list)))
		      #'(lambda (,a ,n)
			  (declare (ignorable ,a ,n))
			  ,@(sort-ref-series body-list)))))))

(defun split-list (lst index)
  (let ((target (if (null index) 0 (1+ index))))
    (list (subseq lst 0 target)
	  (nthcdr target lst))))

(defun split-by-last (lst delimiter)
  (split-list lst
	      (position delimiter lst :from-end t)))

; #{a n} -> (lnth n a)
(defun {-reader (stream &rest rest)
  (declare (ignore rest))
  (let ((*readtable* (copy-readtable *readtable*))
	(pair nil))
    (set-macro-character #\} (get-macro-character #\)))
    (set-macro-character #\[ #'[-reader)
    (setf pair (read-delimited-list #\} stream t))
    (labels ((recursive-lnth (lst)
	       (if (null (cdr lst))
		   (car lst)
		   `(lnth ,(car lst) ,(recursive-lnth (cdr lst))))))
      (recursive-lnth (reverse pair)))))
  
; Ex. #[n-1] -> (- n 1)
(defun [-reader (stream &rest rest)
  (declare (ignore rest))
  (let ((lst nil)
	(*readtable* (copy-readtable *readtable*)))
    ; TODO: commonize these similar descriptions
    (set-separate-character #\-)
    (set-separate-character #\*)
    (set-separate-character #\+)
    (set-separate-character #\/)
    (set-macro-character #\] (get-macro-character #\)))
    (setf lst (read-delimited-list #\] stream t))
    (case (length lst)
      (1 (car lst))
      (3 (case (cadr lst)
	   (#\- `(- ,(car lst) ,(caddr lst)))
	   (#\+ `(+ ,(car lst) ,(caddr lst)))
	   (#\/ `(/ ,(car lst) ,(caddr lst)))
	   (#\* `(* ,(car lst) ,(caddr lst)))))
      (t (error 'simple-error)))))
  
(defun set-separate-character (char)
  (set-macro-character char
		       #'(lambda (s c)
			   (declare (ignore s c))
			   char)))
