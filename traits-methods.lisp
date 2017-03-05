;;;;; Methods for working with trais

(in-package traits)

(defmethod draw ((obj can-draw) &optional (offset (cons 0 0)))
  "Draw object with optional offset"
  (io:draw-char (image obj)
		(+ (x obj) (car offset))
		(+ (y obj) (cdr offset))))

(defmethod draw ((obj can-draw-colour) &optional (offset (cons 0 0)))
  (with-colour (colour obj)
    (io:draw-char (image obj)
		  (+ (x obj) (car offset))
		  (+ (y obj) (cdr offset)))))

(defmethod move ((obj can-move) mod-x mod-y)
  "Move object by mod-x and mod-y"
  (incf (x obj) mod-x)
  (incf (y obj) mod-y))

(defmethod is-here? ((obj has-place) x y)
  "True if object occupies coordinates x and y, else false"
  (and (= (x obj) x)
       (= (y obj) y)))

(defmethod attack ((a can-attack) (b has-combat-attributes))
  (format t "~A attacked ~A" (identify a) (identify b)))

(defmethod dead? ((object has-combat-attributes))
  (< (health object) 0))

(defmethod identify (obj)
  "Unknown obj")

(defmethod identify ((obj has-id))
  (id obj))
