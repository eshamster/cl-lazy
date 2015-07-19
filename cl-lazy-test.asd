#|
  This file is a part of cl-lazy project.
  Copyright (c) 2015 eshamster (hamgoostar@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-lazy-test-asd
  (:use :cl :asdf))
(in-package :cl-lazy-test-asd)

(defsystem cl-lazy-test
  :author "eshamster"
  :license ""
  :depends-on (:cl-lazy
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "cl-lazy"))))

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
