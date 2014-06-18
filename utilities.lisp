(defpackage :project-euler-utilities
  (:use :cl)
  (:export :make-list1+
           :!
           :fib-true
           :factor
           :mkstr
           :palindromep
           :member1
           :prime-list
           :prime-reset
           :prime-show
           :pythagorean
           :prime))

(in-package :project-euler-utilities)

;; added to solve problem 1
(defun make-list1+ (size &optional (element 0) (cnt 0) (new-list (list)))
  (cond
    ((>= cnt size) new-list)
    (t (make-list1+ size (+ element 1) (1+ cnt) (append new-list (list element))))))

;; added because '!' command does not exists in SBCL Lisp dialect
#+sbcl (defun ! (x)
         "A simple factorial implementation for the SBCL Lisp dialect."
         (labels ((helper (current &optional (accum 1))
                    (if (= current 1)
                      accum
                      (helper (1- current) (* current accum)))))
           (if (and (integerp x) (> x 0))
             (helper x)
             (error (format nil "argument ~A is not a nonnegative fixnum" x)))))

;; added to solve problem 2
(defun mklist (obj)
  "Returns the input obj enclosed in a list if the obj is not already a list."
  (if (listp obj) obj (list obj)))

(defun fib (terms &optional (fib0 1) (fib1 1) (cnt 1) (new-list (list)) &key (filter #'integerp))
  "Attempt at making a 'functional' Fibonacci generator."
  (cond
    ((= cnt 1) (if (= cnt terms)
                 (if (funcall filter fib0) (mklist fib0))
                 (if (funcall filter fib0)
                   (fib terms fib0 fib1 (1+ cnt) (mklist fib0) :filter filter)
                   (fib terms fib0 fib1 (1+ cnt) new-list :filter filter))))
    ((> cnt terms) new-list)
    (t (if (funcall filter fib1)
         (fib terms fib1 (+ fib0 fib1) (1+ cnt) (append new-list (mklist fib1)) :filter filter)
         (fib terms fib1 (+ fib0 fib1) (1+ cnt) new-list :filter filter)))))

;; added to solve problem 2
(defun fib-h (terms &key (filter #'integerp))
  "Attempt at creating a helper function."
  (fib terms 1 1 1 (list) :filter filter))

;; added to solve problem 2
(defun fib-true (terms &key (filter #'integerp))
  "Another attempt at making a 'functional' Fibonacci generator."
  (labels ((fib-h-true (terms &optional (fib0 1) (fib1 1) (cnt 1) (new-list (list)) &key (filter #'integerp))
             (cond
               ((= cnt 1) (if (= cnt terms)
                            (if (funcall filter fib0) (mklist fib0))
                            (if (funcall filter fib0)
                              (fib-h-true terms fib0 fib1 (1+ cnt) (mklist fib0) :filter filter)
                              (fib-h-true terms fib0 fib1 (1+ cnt) new-list :filter filter))))
               ((> cnt terms) new-list)
               (t (if (funcall filter fib1)
                    (fib-h-true terms fib1 (+ fib0 fib1) (1+ cnt) (append new-list (mklist fib1)) :filter filter)
                    (fib-h-true terms fib1 (+ fib0 fib1) (1+ cnt) new-list :filter filter))))))
    (fib-h-true terms 1 1 1 (list) :filter filter)))

;; added to solve problem 3
(defun factor (num &optional (cnt 2) (new-list (list)))
  (cond
    ((> cnt num) new-list)
    ((= (mod num cnt) 0) (factor (/ num cnt) cnt (append new-list (mklist cnt))))
    (t (factor num (1+ cnt) new-list))))

;; added to solve problem 4
(defun mkstr (&rest args)
  "This is not my code. I got this from a Lisp programming book. I will provide the details whenever I come back to this function.
  Returns any provided arguments as a single string."
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

;; added to solve problem 4
(defun palindromep (num)
  "Tests the provided number as palindromic in nature."
  (let ((str (mkstr num)))
    (equal 
      (subseq str (ceiling (/ (length str) 2)) (length str))
      (reverse (subseq str 0 (floor (/ (length str) 2)))))))

;; added to solve problem 5
(defun member1 (my-list element &optional (new-list (list)))
  (cond
    ((eq my-list nil) new-list)
    ((equal (car my-list) element) (append new-list (cdr my-list)))
    (t (member1 (cdr my-list) element (append new-list (mklist (car my-list)))))))

;; added to solve problem 7
(let ((p-array (make-array 1 :initial-element 2 :fill-pointer t :adjustable t)))
  (defun prime-list (term &optional (cnt 2) (index 0))
    (cond
      ((= cnt 2) (if (= (aref p-array (1- (length p-array))) 2)
                   (prime-list term 3 index)
                   (prime-list term (+ (aref p-array (1- (length p-array))) 2) 0)))
      ((>= (length p-array) term) (aref p-array (1- term)))
      ((or (>= index (length p-array)) (> (aref p-array index) (ceiling (/ cnt 2)))) (vector-push-extend cnt p-array)
                                                                                     (prime-list term (+ cnt 2) 0))
      ((= (mod cnt (aref p-array index)) 0) (prime-list term (+ cnt 2) 0))
      (t (prime-list term cnt (1+ index)))))
  (defun prime-reset ()
    (setf p-array (make-array 1 :initial-element 2 :fill-pointer t :adjustable t)))
  (defun prime-show () p-array))

;; added to solve problem 9
(defun pythagorean (a b)
  (sqrt (+ (expt a 2) (expt b 2))))

;; added to solve problem 10
(defun prime (term &optional (pp-array (make-array term :initial-element nil)))
  (labels ((quad0 (x y) (+ (* 4 (expt x 2)) (expt y 2)))
           (quad1 (x y) (+ (* 3 (expt x 2)) (expt y 2)))
           (quad2 (x y) (- (* 3 (expt x 2)) (expt y 2)))
           (helper0 (limit my-array &optional (cnt 0))
             (cond
               ((>= cnt limit) my-array)
               (t (setf (svref my-array cnt) (mod cnt 60))
                  (helper0 limit my-array (1+ cnt)))))
           (helper1 (limit my-array &optional (x 1) (y 1) (p-array (make-array limit :initial-element nil)))
             (cond
               ((> x (sqrt limit)) (setf (svref p-array 2) 2)
                                   (setf (svref p-array 3) 3)
                                   (setf (svref p-array 5) 5)
                                   p-array)
               ((> y (sqrt limit)) (helper1 limit my-array (1+ x) 1 p-array))
               (t (let ((v-quad0 (quad0 x y)) (v-quad1 (quad1 x y)) (v-quad2 (quad2 x y)))
                    (if (and (integerp v-quad0) (< v-quad0 limit))
                      (let ((r (svref my-array v-quad0)))
                        (if (or (= r 1) (= r 13) (= r 17) (= r 29) (= r 37) (= r 41) (= r 49) (= r 53))
                          (setf (svref p-array v-quad0) v-quad0))))
                    (if (and (integerp v-quad1) (< v-quad1 limit))
                      (let ((r (svref my-array v-quad1)))
                        (if (or (= r 7) (= r 19) (= r 31) (= r 43))
                          (setf (svref p-array v-quad1) v-quad1))))
                    (if (and (> x y) (integerp v-quad2) (< v-quad2 limit))
                      (let ((r (svref my-array v-quad2)))
                        (if (or (= r 11) (= r 23) (= r 47) (= r 59))
                          (setf (svref p-array v-quad2) v-quad2))))
                    (helper1 limit my-array x (1+ y) p-array)))))
           (helper2 (limit my-array &optional (index 0) (coe 3))
             (cond
               ((>= index limit) my-array)
               ((and (not (null (svref my-array index))) (>= (* coe (svref my-array index)) limit)) (helper2 limit my-array (1+ index) 3))
               ((not (null (svref my-array index))) (if (not (null (svref my-array (* coe (svref my-array index)))))
                                                      (setf (svref my-array (* coe (svref my-array index))) nil))
                                                    (helper2 limit my-array index (+ 2 coe)))
               (t (helper2 limit my-array (1+ index) coe)))))
    (remove-if #'null (helper2 term (helper1 term (helper0 term pp-array))))))
