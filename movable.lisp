(defpackage movable
  (:use :cl :drawable)
  (:export mover
	   move))

(in-package movable)

(defclass mover (tile) ())

(defmethod move ((mover mover) x-mod y-mod level player monsters)
  "Move mover for x-mod in x and for y-mod in y"
  (unless (and (= x-mod 0)
	       (= y-mod 0))
    (let* ((new-x (+ (x mover) x-mod))
	   (new-y (+ (y mover) y-mod))
	   (here-monster (drawable:are-here? monsters new-x new-y)))
      (cond ((level:blocked? (level:get-tile level new-x new-y)) nil)
	    ((drawable:is-here? player new-x new-y) (combat:attack mover player))
	    (here-monster (combat:attack player here-monster))
	    (t (progn
		 (setf (x mover) new-x)
		 (setf (y mover) new-y)))))))
