; HELP FUNCTIONS -------------------------------------------------------------

(defun object-type (obj)
  (car obj))

(defun set-object-type (obj obj-type)
  (setf (car obj) obj-type)
  obj)

(defun object-value (obj)
  (caddr obj))

(defun set-object-value (obj val)
  (setf (caddr obj) val)
  obj)

(defun object-create (obj-type color val)
  (list obj-type color val))

(defun test-type (obj obj-type)
  (if (typep obj-type 'list)
    (if (not (eql (car obj-type) (object-type obj)))
      (test-type obj (cdr obj-type))
      T)
    (unless (eql (object-type obj) obj-type)
      (error "Type error"))))

; CONTRUCTORS ----------------------------------------------------------------

(defun make-point ()
  (object-create 'point :black (list 0 0)))

(defun make-circle ()
  (let ((point-obj (make-point))
        (obj (object-create 'circle :black (list 1))))
    (set-object-value obj (append (list point-obj) (object-value obj)))
    obj))

(defun make-polygon ()
  (object-create 'polygon :black (list)))

(defun make-picture ()
  (object-create 'picture :black (list)))

; MOVE -----------------------------------------------------------------------

(defun move (obj dx dy)
  (let ((obj-type (object-type obj))
        (obj-val (object-value obj)))
    (progn
      (if (eql obj-type 'point)
        (progn
          (set-x obj (+ (x obj) dx))
          (set-y obj (+ (y obj) dy))
          obj) 
        (if (eql obj-type 'circle)
          (move (car (object-value obj)) dx dy)
        (if (or (eql obj-type 'polygon) (eql obj-type 'picture))
          (mapcar (lambda (val) (move val dx dy)) obj-val)
          (error "Unknown object type"))))
    obj)))

; COLOR ----------------------------------------------------------------------

(defun color (struct)
  (cadr struct))

(defun set-color (struct color)
  (setf (cadr struct) color)
  struct)

; POINT FUNCTIONS ------------------------------------------------------------

(defun x (point)
  (test-type point '(point circle))
  (if (eql (object-type point) 'circle)
    (x (car (object-value point)))
    (car (object-value point))))

(defun y (point)
  (test-type point '(point circle))
  (if (eql (object-type point) 'circle)
    (y (car (object-value point)))
    (cadr (object-value point))))

(defun set-x (point x)
  (test-type point '(point circle))
  (if (eql (object-type point) 'circle)
    (set-x (car (object-value point)) x)
    (setf (car (object-value point)) x))
  point)

(defun set-y (point y)
  (test-type point '(point circle))
  (if (eql (object-type point) 'circle)
    (set-y (car (object-value point)) y)
    (setf (cadr (object-value point)) y))
  point)

; CIRCLE FUNCTIONS -----------------------------------------------------------

(defun radius (circle)
  (test-type circle 'circle)
  (cadr (object-value circle)))

(defun set-radius (circle val)
  (test-type circle 'circle)
  (setf (cadr (object-value circle)) val)
  circle)

; POLOYGON/PICTURE FUNCTIONS -------------------------------------------------

(defun items (obj)
  (test-type obj (list 'polygon 'picture))
  (object-value obj))

(defun set-items (obj items)
  (test-type obj (list 'polygon 'picture))
  (setf (caddr obj) items)
  obj)
