(defpackage drawable
  (:use :cl)
  (:export tile
	   draw
	   is-here?
	   are-here?
	   
	   x
	   y))

(in-package drawable)

(defclass tile ()
  ((x     :accessor   x
          :initarg   :x)
   (y     :accessor   y
          :initarg   :y)
   (image :reader     image
	  :initarg   :image)))

(defmethod draw ((tile tile) &optional (offset (cons 0 0)))
  "Draw tile to window"
  (io:draw-char (image tile) (x tile) (y tile) offset))

(defmethod is-here? ((tile tile) x y)
  "Return tile if it's on given coordinates, else nil"
  (if (and (= x (x tile))
	   (= y (y tile)))
      tile
      nil))

(defun are-here? (list x y)
  "Return first tile that is on given coordinates from list"
  (if (null list)
      nil
      (if (is-here? (car list) x y)
	  (car list)
	  (are-here? (cdr list) x y))))
