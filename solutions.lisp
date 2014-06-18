(defpackage :project-euler
  (:use :cl :project-euler-utilities))

(in-package :project-euler)

(defun hello-world (&optional (name "World"))
  (print (concatenate 'string "Hello, " name "!!!")))

(defun SolveProb1 ()
  (apply #'+
         (remove-if (complement #'(lambda (x) 
                                    (if (or (= (mod x 3) 0) 
                                            (= (mod x 5) 0)) 
                                        t))) 
                    (make-list1+ 1000 1))))
