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

(defun SolveProb2 ()
  (apply #'+ (fib-true 33 :filter #'evenp)))

(defun SolveProb3 ()
  (car (sort (factor 600851475143) #'>)))

(defun SolveProb4 ()
  (labels ((helper (num0 num1 &optional (my-max 0))
             (cond
               ((> (length (mkstr num0)) 3) my-max)
               ((and (palindromep (* num0 num1)) (> (* num0 num1) my-max)) (helper (1- num1) (1- num1) (* num0 num1)))
               ((and (> my-max 0) (<= (* num0 num1) my-max)) (helper (1- num1) (1- num1) my-max))
               ((<= num0 1) (helper (1- num1) (1- num1) my-max))
               (t (helper (1- num0) num1 my-max)))))
    (helper 999 999)))

(defun SolveProb5 ()
  (labels ((helper (factors &optional (product 1))
             (cond
               ((eq factors nil) product)
               ((eq (car factors) nil) (helper (cdr factors) product))
               (t (helper (mapcar #'(lambda (x) (member1 x (caar factors))) factors) (* product (caar factors)))))))
    (helper (mapcar #'factor (make-list1+ 19 2)))))

(defun SolveProb6 ()
  (labels ((sqr (num)
             (* num num)))
    (abs (- (apply #'+ (mapcar #'sqr (make-list1+ 100 1))) (sqr (apply #'+ (make-list1+ 100 1)))))))

(defun SolveProb7 ()
  (prime-list 10001))

(defun SolveProb8 ()
  (labels ((helper (temp &optional (index 0) (my-max 0))
             (cond
               ((>= (+ index 5) (length temp)) my-max)
               ((> (* (parse-integer temp :start index :end (+ index 1))
                      (parse-integer temp :start (+ index 1) :end (+ index 2))
                      (parse-integer temp :start (+ index 2) :end (+ index 3))
                      (parse-integer temp :start (+ index 3) :end (+ index 4))
                      (parse-integer temp :start (+ index 4) :end (+ index 5)))
                    my-max) (helper temp (1+ index) (* (parse-integer temp :start index :end (+ index 1))
                                                       (parse-integer temp :start (+ index 1) :end (+ index 2))
                                                       (parse-integer temp :start (+ index 2) :end (+ index 3))
                                                       (parse-integer temp :start (+ index 3) :end (+ index 4))
                                                       (parse-integer temp :start (+ index 4) :end (+ index 5)))))
               (t (helper temp (1+ index) my-max)))))
    (helper (mkstr (with-open-file (stream "Prob8.txt") (read stream))))))

(defun SolveProb9 ()
  (labels ((helper (value &optional (a 3) (b 4))
             (cond
               ((> a (- value 2)) nil)
               ((> b (1- value)) (helper value (1+ a) (+ a 2)))
               ((and (integerp (pythagorean a b)) (= (+ a b (pythagorean a b)) value)) (list a b (pythagorean a b)))
               (t (helper value a (1+ b))))))
    (apply #'* (helper 1000))))

(defun SolveProb10 ()
  (reduce #'+ (prime 2000000)))
