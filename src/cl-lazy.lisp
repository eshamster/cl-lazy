#|--------
  Basic
  --------|#
(in-package :cl-user)
(defpackage cl-lazy
  (:use :cl
        :anaphora)
  (:import-from :alexandria
                :with-gensyms))
(in-package :cl-lazy)

(cl-annot:enable-annot-syntax)

@export
(defmacro lazy (&body body)
  (with-gensyms (value)
    (if (and (= (length body) 1)
             (atom (car body)))
        (car body)
        `(let ((,value nil))
           (lambda ()
             (unless ,value
               (setf ,value (progn ,@body)))
             ,value)))))

@export
(defun force (lazy-value)
  (if (functionp lazy-value)
      (funcall lazy-value)
      lazy-value))

@export
(defmacro lcons (a b)
  `(cons (lazy ,a) (lazy ,b)))

@export
(defmacro lcar (l-lst)
  `(force (car ,l-lst)))

@export
(defmacro lcdr (l-lst)
  `(progn
     (force (car ,l-lst))
     (force (cdr ,l-lst))))

@export
(defmacro llist (&rest rest)
  (llist-body rest))

;; (llist-body '(1 2 3) :tail tail) -> (LCONS 1 (LCONS 2 (LCONS 3 tail)))
(defun llist-body (lst &key (tail nil))
  (labels ((f (llst rest-arg)
	     (if (null rest-arg)
		 llst
		 (f `(lcons ,(car rest-arg) ,llst) (cdr rest-arg)))))
    (f tail (reverse lst))))

@export
(defun llist-to-list (llst &key (max-length -1) (stops-at-nil t))
  (labels ((f (lst rest-llst rest-length)
	     (if (or (null rest-llst)
		     (and stops-at-nil (null (lcar rest-llst)))
		     (= rest-length 0))
		 lst
		 (f (cons (lcar rest-llst) lst)
		    (lcdr rest-llst)
		    (1- rest-length)))))
    (nreverse (f nil llst max-length))))

@export
(defun list-to-llist (lst)
  (labels ((rec (rest-lst)
             (when rest-lst
               (lcons (car rest-lst)
                      (rec (cdr rest-lst))))))
    (rec lst)))

@export
(defun lnth (n l-lst)
  (lcar (lnthcdr n l-lst)))

@export
(defun lnthcdr (n l-lst)
  (if (<= n 0)
      l-lst
      (lnthcdr (1- n) (lcdr l-lst))))

@export
(defmacro do-llist ((value llst) &body body)
  (with-gensyms (f rest)
    `(labels ((,f (,rest)
		(when ,rest
                  (let ((,value (lcar ,rest)))
                    ,@body
                    (,f (lcdr ,rest))))))
       (,f ,llst))))

@export
(defun lreverse (llst)
  (labels ((rec (result rest-llst)
             (aif (lcar rest-llst)
                  (rec (lcons it result) (lcdr rest-llst))
                  result)))
    (rec nil llst)))

@export
(defun lspan (predicate llst)
  "Extract from llst until the predication is failed. The return value 1 is the extracted lazy list, and the value 2 is the rest lazy list"
  (let (rest)
    (labels ((rec-find (ok rest-llst)
               (alet (lcar rest-llst)
                 (if (and rest-llst
                          (funcall predicate it))
                     (rec-find (cons it ok) (lcdr rest-llst))
                     (progn (setf rest rest-llst)
                            (list-to-llist (nreverse ok)))))))
      (values (rec-find nil llst) rest))))

@export
(defmacro lappend (&rest llsts)
  `(lappend-body (llist ,@llsts)))

(defun lappend-body (llsts)
  (labels ((get-next (&optional (target-llst (lcar llsts))
                                (rest-llsts (lcdr llsts)))
             (acond ((lcar target-llst)
                     (lcons it (get-next (lcdr target-llst) rest-llsts))) 
                    ((lcdr rest-llsts)
                     (get-next (lcar rest-llsts) it))
                    (t (lcar rest-llsts)))))
    (get-next)))

#|
Ex1. Series of even numbers -> [0, 2, 4, 6, ...]
(make-series nil #'(lambda (a n) (* n 2)))

Ex2. Fibonacci series -> [0, 1, 1, 2, 3, 5, 8, 13, ...]
(make-series '(0 1) #'(lambda (a n) (+ (lnth (- n 1) a) (lnth (- n 2) a)))
             |#
@export
(defun make-series (init-nums fn-calc)
  (let (a)
    (labels ((f (n) (lcons (funcall fn-calc a n) (f (1+ n)))))
      (setf a (recursive-lcons init-nums (f (length init-nums)))))))

@export
(defun make-simple-series (init-nums fn-calc)
  "Ex. (make-simple-series '(3 4) (lambda (n) (* n 5))) -> [3, 4, 10, 15, 20, ...]"
  (labels ((f (n) (lcons (funcall fn-calc n) (f (1+ n)))))
    (recursive-lcons init-nums (f (length init-nums)))))

@export
(defun liota (&optional (start 0) (step 1) (count -1))
  (labels ((rec (value rest-count)
             (when (/= rest-count 0)
               (lcons value (rec (+ value step) (1- rest-count))))))
    (rec start count)))

(defun recursive-lcons (lst tail)
  (if (null lst)
      tail
      (lcons (car lst) (recursive-lcons (cdr lst) tail))))

#|-------
  Utils
  -------|#

@export
(defun is-series-end (series)
  (null (lcar series)))

@export
(defmacro do-series ((value series to last-index) &body body)
  (unless (equal (symbol-name to) "TO")
    (error 'simple-error))
  (with-gensyms (counter last)
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
  (with-gensyms (n)
    `(make-simple-series
      nil
      #'(lambda (,n)
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
  (let ((rest-series series))
    (make-series
     nil
     #'(lambda (a n)
	 (labels ((get-next (&optional (rest-dist give-up-distance))
                    (when (<= rest-dist 0)
                      (error "A target value didn't be found in ~D span" give-up-distance))
                    (let ((target (lcar rest-series)))
                      (setf rest-series (lcdr rest-series))
                      (if (funcall fn-find target a n)
                          target
                          (get-next (1- rest-dist))))))
           (get-next))))))

#|-------------
  Reader Macro
  -------------|#
@export
(defun enable-series-processing-syntax ()
  (let ((old-table (copy-readtable *readtable*)))
    (setf *readtable* (add-readtable))
    old-table))

(defun add-readtable ()
  (let ((*readtable* (copy-readtable *readtable*)))
    (set-dispatch-macro-character #\# #\< #'<-reader)
    (set-dispatch-macro-character #\# #\{ #'{-reader)
    (set-dispatch-macro-character #\# #\[ #'[-reader)
    *readtable*))

;; This enables to make a series as
;; #<a[n] = 0, 1, (+ a[n-1] a[n-2])>
(defun <-reader (stream &rest rest)
  (declare (ignore rest))
  (let ((*readtable* (copy-readtable *readtable*))
	buf a n)
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
	       But if finds (a (#\[ b)), sorts this to (lnth b a).
	       |#
	       (let (res)
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

;; #{a n} -> (lnth n a)
;; #{a n k} -> (lnth k (lnth n a))
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

;; Ex. #[n-1] -> (- n 1)
(defun [-reader (stream &rest rest)
  (declare (ignore rest))
  (let ((lst nil)
	(*readtable* (copy-readtable *readtable*)))
    ;; TODO: commonize these similar descriptions
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
