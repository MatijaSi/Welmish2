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

;;;; TILE

(defclass level-tile (can-draw-colour
		      has-place
		      has-light-blocking
		      has-blocking
		      is-visitable) ())

(defmethod draw/fov ((tile level-tile) player &optional (offset (cons 0 0)))
  "Draw level tile if it's in player fov, else if visited call draw/fov-out"
  (cond ((and (> 5 (abs (- (x tile) (x player))))
	      (> 5 (abs (- (y tile) (y player)))))
	 (progn
	   (unless (visited? tile)
	     (setf (visited? tile) t))
	   (draw tile offset)))
	((visited? tile) (draw/fov-out tile offset))))

(defmethod draw/fov-out ((tile level-tile) &optional (offset (cons 0 0)))
  "Draw visited level tile if it isn't in fov."
  (with-color +blue+
	      (io:draw-char (image tile)
			    (+ (x tile) (car offset))
			    (+ (y tile) (cdr offset)))))
		
	
(defun make-ground-tile (x y)
  "Generate a ground with coords x y"
  (make-instance 'level-tile
		 :x x
		 :y y
		 :image #\.
		 :colour io:+cyan+
		 :light-blocked nil
		 :blocked nil))

(defun make-wall-tile (x y)
  "Generate a wall with coords x y"
  (make-instance 'level-tile
		 :x x
		 :y y
		 :image #\#
		 :colour io:+cyan+
		 :light-blocked t
		 :blocked t))

;;;; LEVEL

;;; Object for holding game map.
(defclass level ()
  ((height :reader   height
	   :initarg :h)
   (width  :reader   width
	   :initarg :w)
   (grid   :accessor grid
	   :initarg :grid)))

(defmethod map-level (fun (level level))
  "Apply fun to all elements of level"
  (do ((x 0 (+ x 1)))
      ((= x (width level)) level)
    (do ((y 0 (+ y 1)))
	((= y (height level)) 'done)
      (funcall fun (aref (grid level) x y)))))

(defmethod map-level-row (fun (level level) y)
  "Apply fun to all elements of row y"
  (do ((x 0 (+ x 1)))
      ((= x (width level)) level)
    (funcall fun (get-tile level x y))))

(defmethod map-level-row\coords (fun (level level) y)
  "Apply fun to all coords of row y"
  (do ((x 0 (+ x 1)))
      ((= x (width level)) level)
    (funcall fun x y)))

(defmethod map-level-column (fun (level level) x)
  "Apply fun to all elements of column x"
  (do ((y 0 (+ y 1)))
      ((= y (height level)) level)
    (funcall fun (get-tile level x y))))

(defmethod map-level-column\coords (fun (level level) x)
  "Apply fun to all coords of column x"
  (do ((y 0 (+ y 1)))
      ((= y (height level)) level)
    (funcall fun x y)))

(defmethod map-level-coords (fun (level level))
  "Apply fun to all elements of level, pass it their coords too"
  (do ((x 0 (+ x 1)))
      ((= x (width level)) level)
    (do ((y 0 (+ y 1)))
	((= y (height level)) 'done)
      (funcall fun (aref (grid level) x y) x y))))

(defmethod get-tile ((level level) x y)
  "Get tile from level with coords x y"
  (aref (grid level) x y))

(defmethod get-random ((level level))
  "Get random tile from level"
  (get-tile level
	    (random (width level))
	    (random (height level))))
