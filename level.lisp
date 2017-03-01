(defpackage level
  (:use :cl)
  (:export level
	   level-tile

	   blocked?
	   
	   grid
	   height
	   width

	   draw
	   
	   make-ground-tile
	   make-wall-tile
	   
	   map-level
	   map-level-coords

	   map-level-row
	   map-level-row\coords

	   map-level-column
	   map-level-column\coords
	   
	   get-tile))

(in-package level)

(defclass level-tile (drawable:tile)
  ((blocked? :reader   blocked?
	     :initarg :blocked?)))

;; A game map class
(defclass level ()
  ((height :reader   height
	   :initarg :h)
   (width  :reader   width
	   :initarg :w)
   (grid   :accessor grid
	   :initarg :grid)))

(defun make-ground-tile (x y)
  (make-instance 'level-tile :x x :y y :blocked? nil :image #\.))

(defun make-wall-tile (x y)
  (make-instance 'level-tile :x x :y y :blocked? t :image #\#))

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
  (aref (grid level) x y))

(defmethod draw ((level level) &optional (offset (cons 0 0)))
  (map-level #'(lambda (tile) (drawable:draw tile offset)) level))

