(defpackage drawable
  (:use :cl)
  (:export tile
	   draw

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
