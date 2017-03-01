;;;;; Methods for working with trais

(defpackage traits-methods
  (:use :cl :traits))

(in-package traits-methods)

(defmethod draw ((obj can-draw) &optional (offset (cons 0 0)))
  "Draw object with optional offset"
  (io:draw-char (image obj)
		(+ (x obj) (car offset))
		(+ (y obj) (cdr offset))))

(defmethod move ((obj can-move) mod-x mod-y)
  "Move object by mod-x and mod-y"
  (incf (x obj) mod-x)
  (incf (y obj) mod-y))

(defmethod is-here? ((obj has-place) x y)
  "True if object occupies coordinates x and y, else false"
  (and (= (x obj) x)
       (= (y obj) y)))

(defmethod attack ((a can-attack) (b has-combat-attributes))
  ...)

(defmethod dead? ((object has-combat-attributes))
  (< (health object) 0))
