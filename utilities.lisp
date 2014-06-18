(defpackage :project-euler-utilities
  (:use :cl)
  (:export :make-list1+
           :!))

(in-package :project-euler-utilities)

(defun make-list1+ (size &optional (element 0) (count 0) (new-list (list)))
  (cond
    ((>= count size) new-list)
    (t (make-list1+ size (+ element 1) (1+ count) (append new-list (list element))))))

#+sbcl (defun ! (x)
         (labels ((helper (current &optional (accum 1))
                    (if (= current 1)
                      accum
                      (helper (1- current) (* current accum)))))
           (if (and (integerp x) (> x 0))
             (helper x)
             (error (format nil "argument ~A is not a nonnegative fixnum" x)))))


