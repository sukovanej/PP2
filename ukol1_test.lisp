(in-package :cl-user)
(defpackage ukol1-test
  (:use :cl
        :prove))
(in-package :ukol1-test)

(load "milan-suk-1.lisp")

(defun make-point-2 (x y)
  (let ((obj (make-point)))
    (set-x obj x)
    (set-y obj y)
    obj))

(subtest "make-point-2"
         (is (object-value (make-point-2 1 2)) '(1 2)))

(subtest "object"
         (let ((x (object-create 'a :c 12)))
         (is x (list 'a :c 12))
         (is (object-value x) 12)
         (is (color x) :c)
         (is (object-type x) 'a)
         ))

(subtest "make-point"
         (defvar p (make-point))
         (is (car p) 'point)
         (is (cadr p) :black)
         (is (caddr p) (list 0 0)))

(subtest "make-circle"
         (let ((c (make-circle)))
           (is (object-type c) 'circle)
           (is (color c) :black)
           (is (object-value c) (list (make-point-2 0 0) 1))))

(subtest "move"
         (let ((c1 (make-circle))
               (p1 (make-point))
               (i1 (make-picture))
               (l1 (make-polygon)))
           (move c1 2 1)
           (is (object-value c1) (list (make-point-2 2 1) 1))
           (move p1 1 1)
           (is (object-value p1) (list 1 1))
           (set-items l1 (list (make-point-2 1 1) (make-point-2 2 2)))
           (move l1 1 2)
           (is (items l1) (list (make-point-2 2 3) (make-point-2 3 4)))

           (set-items i1 (list c1 p1 l1))
           (move i1 1 1)
           (is (car (object-value (nth 0 (items i1)))) (make-point-2 3 2))
           (is (nth 1 (items i1)) (make-point-2 2 2))
           (is (object-value (nth 2 (items i1))) (list (make-point-2 3 4) (make-point-2 4 5)))
           ))

(subtest "color"
         (let ((a (make-point))
               (b (make-circle))
               (c (make-polygon))
               (d (make-picture)))
           (is (color a) :black)
           (is (color b) :black)
           (is (color c) :black)
           (is (color d) :black)))

(subtest "circle"
         (let ((c1 (make-circle)))
           (is (radius c1) 1)
           (set-radius c1 5)
           (is (radius c1) 5)
           (set-x c1 1)
           (set-y c1 2)
           (is (x c1) 1)
           (is (y c1) 2)))

(subtest "point"
         (let ((p1 (make-point)))
           (is (x p1) 0)
           (is (y p1) 0)
           (set-x p1 1)
           (is (x p1) 1)
           (set-y p1 1)
           (is (y p1) 1)))

(finalize)
