(defpackage movable
  (:use :cl)
  (:export mover
	   move))

(in-package movable)

(defclass mover (tile)
  ((x :accessor x
      :initarg :x)
   (y :accessor y
      :initarg :y)))

(defmethod move ((mover mover) x-mod y-mod level)
  "Move mover for x-mod in x and for y-mod in y"
  (let ((new-x (+ (x mover) x-mod))
	(new-y (+ (y mover) y-mod)))
    (unless (blocked? (get-tile level new-x new-y))
      (setf (x mover) new-x)
      (setf (y mover) new-y))))
