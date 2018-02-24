(in-package :cl-user)
(defpackage ukol1-test
  (:use :cl
        :prove))
(in-package :ukol1-test)

(load "ukol1.lisp")

(subtest "object"
         (let ((x (object-create 'a :c 12)))
         (is x (list 'a :c 12))
         (is (object-value x) 12)
         (is (object-color x) :c)
         (is (object-type x) 'a)
         ))

(subtest "make-point"
         (defvar p (make-point))
         (is (car p) 'point)
         (is (cadr p) :black)
         (is (caddr p) (list 0 0)))

(subtest "make-circle"
         (let ((c (make-circle)))
           (is (car c) 'circle)
           (is (cadr c) :black)
           (is (caddr c) (list 0 0 1))))

(subtest "move"
         (let ((c1 (make-circle))
               (p1 (make-point))
               (i1 (make-picture))
               (l1 (make-polygon)))
           (move c1 2 1)
           (is (object-value c1) (list 2 1 1))
           (move p1 1 1)
           (is (object-value p1) (list 1 1))
           (set-items l1 (list (list 1 2) (list 2 3) (list 4 5)))
           (move l1 1 1)
           (is (items l1) '((2 3) (3 4) (5 6)))
           (set-items i1 (list c1 p1 l1))
           (move i1 1 1)
           (is (object-value (nth 0 (items i1))) '(3 2 1))
           (is (object-value (nth 1 (items i1))) '(2 2))
           (is (object-value (nth 2 (items i1))) '((3 4) (4 5) (6 7)))
           ))

(subtest "circle"
         (let ((c1 (make-circle)))
           (set-radius c1 5)
           (is (radius c1) 5)))

(subtest "point"
         (let ((p1 (make-point)))
           (is (x p1) 0)
           (is (y p1) 0)
           (set-x p1 1)
           (is (x p1) 1)
           (set-y p1 1)
           (is (y p1) 1)))

(finalize)
