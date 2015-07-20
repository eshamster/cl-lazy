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

; (llist-body '(1 2 3)) -> (LCONS 1 (LCONS 2 (LCONS 3 NIL)))
(defun llist-body (lst)
  (labels ((f (llst rest-arg)
	     (if (null rest-arg)
		 llst
		 (f (cons 'lcons (list (car rest-arg) llst)) (cdr rest-arg)))))
    (f nil (reverse lst))))

@export
(defun lnth (n l-lst)
  (if (<= n 0)
      (lcar l-lst)
      (lnth (1- n) (lcdr l-lst))))
