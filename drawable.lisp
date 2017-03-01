(defpackage drawable
  (:use :cl)
  (:export tile
	   draw))

(in-package:drawable)

(defclass tile ()
  ((x     :reader   x
          :initarg :x)
   (y     :reader   y
          :initarg :y)
   (image :reader   image
	  :initarg :image)))

(defmethod draw ((tile tile) &optional (offset (cons 0 0)))
  "Draw tile to window"
  (io:draw-char (image tile) (x tile) (y tile) offset))
