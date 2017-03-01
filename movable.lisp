(defpackage movable
  (:use :cl :drawable)
  (:export mover
	   move))

(in-package movable)

(defclass mover (tile) ())

(defmethod move ((mover mover) x-mod y-mod level)
  "Move mover for x-mod in x and for y-mod in y"
  (let ((new-x (+ (x mover) x-mod))
	(new-y (+ (y mover) y-mod)))
    (unless (level:blocked? (level:get-tile level new-x new-y))
      (setf (x mover) new-x)
      (setf (y mover) new-y))))
