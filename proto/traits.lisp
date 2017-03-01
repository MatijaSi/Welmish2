;;;;; Small classes, never used directly, only as parents

(defpackage traits
  (:use :cl))

(in-package traits)

;;;; DRAWING RELATED

(defclass has-coords ()
  (x :accessor x
     :initarg :x)
  (y :accessor y
     :initarg :y))

(defclass has-image ()
  (image :accessor image
	 :initarg :image))

(defclass can-draw (has-image has-coords) ())

(defclass has-colour ()
  (colour :accessor colour
	  :initarg :colour))

(defclass can-draw-colour (has-colour can-draw) ())

;;;; MOVING RELATED

(defclass can-move (has-coords) ())

;;;; LOCATION COLLISION RELATED

(defclass has-place (has-coords) ())

;;;; MAZE AND FOV RELATED

(defclass has-light-blocking ()
  (light-blocked :accessor is-light-blocked?
		 :initarg :light-blocked))

(defclass has-blocking ()
  (blocked :accessor is-blocked?
	   :initarg :blocked))

(defclass visitable ()
  (visited :accessor visited?
	   :initform nil))

;;;; OBJECT BEHAVIOUR RELATED

(defclass player-controlled () ())

(defclass ai-erratic () ())
(defclass ai-hunting () ())
(defclass ai-scaredy () ())

;;;; COMBAT RELATED

(defclass has-combat-attributes ()
  (health   :accessor health
	    :initarg :health)
  (damages  :accessor damages
	    :initarg :damages)
  (defences :accessor defences
            :initarg :defences))

(defclass can-attack (has-combat-attributes) ())
