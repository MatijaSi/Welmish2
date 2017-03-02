(defpackage level
  (:use :cl :traits)
  (:export level

	   grid
	   height
	   width

	   draw

	   make-ground-tile
	   make-wall-tile

	   map-level
	   map-level/coords

	   map-level-row
	   map-level-row/coords

	   map-level-column
	   map-level-column/coords

	   get-tile
	   get-random))

(in-package level)

(defclass level-tile (can-draw-colour
		      has-place
		      has-light-blocking
		      has-blocking
		      is-visitable) ())

(defmethod draw/fov ((tile level-tile) px py)
  (cond ((and (> 5 (abs (- (x tile) px)))
	      (> 5 (abs (- (y tile) py))))
	 (progn
	   (unless (visited? tile)
	     (setf (visited? tile) t))
	   (draw
		
	 

(defun make-ground-tile (x y)
  (make-instance 'level-tile
		 :x x
		 :y y
		 :image #\.
		 :colour :white
		 :light-blocked nil
		 :blocked nil))

(defun make-wall-tile (x y)
  (make-instance 'level-tile
		 :x x
		 :y y
		 :image #\#
		 :colour :white
		 :light-blocked t
		 :blocked t))