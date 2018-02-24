; HELP FUNCTIONS -------------------------------------------------------------

(defun object-type (obj)
  (car obj))

(defun object-value (obj)
  (caddr obj))

(defun object-color (obj)
  (cadr obj))

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
  (object-create 'circle :black (list 0 0 1)))

(defun make-polygon ()
  (object-create 'polygon :black (list)))

(defun make-picture ()
  (object-create 'picture :black (list)))

; MOVE -----------------------------------------------------------------------

(defun move (obj dx dy)
  (let ((obj-type (object-type obj))
        (obj-val (object-value obj)))
    (progn
      (if (or (eql obj-type 'point) (eql obj-type 'circle))
        (progn
          (setf (car obj-val) (+ (car obj-val) dx))
          (setf (cadr obj-val) (+ (cadr obj-val) dy))
          obj)
        (if (eql obj-type 'polygon)
          (mapcar (lambda (val)
                    (progn
                      (setf (car val) (+ (car val) dx))
                      (setf (cadr val) (+ (cadr val) dx))
                      val))
                  obj-val)
          (if (eql obj-type 'picture)
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
  (test-type point 'point)
  (car (object-value point)))

(defun y (point)
  (test-type point 'point)
  (cadr (object-value point)))

(defun set-x (point x)
  (test-type point 'point)
  (setf (car (object-value point)) x)
  point)

(defun set-y (point y)
  (test-type point 'point)
  (setf (cadr (object-value point)) y)
  point)

; CIRCLE FUNCTIONS -----------------------------------------------------------

(defun radius (circle)
  (test-type circle 'circle)
  (caddr (object-value circle)))

(defun set-radius (circle val)
  (test-type circle 'circle)
  (setf (caddr (object-value circle)) val)
  circle)

; POLOYGON/PICTURE FUNCTIONS -------------------------------------------------

(defun items (obj)
  (test-type obj (list 'polygon 'picture))
  (object-value obj))

(defun set-items (obj items)
  (test-type obj (list 'polygon 'picture))
  (setf (caddr obj) items)
  obj)
