;;;;; Small classes, never used directly, only as parents

(defpackage traits
  (:use :cl)
  (:export has-coords
	   has-image
	   
	   can-draw
	   has-colour
	   can-draw-colour
	   
	   can-move
	   has-place
	   
	   has-light-blocking
	   has-blocking
	   is-visitable
	   
	   is-player-controlled
	   has-ai-erratic
	   has-ai-hunting
	   has-ai-scaredy
	   
	   has-combat-attributers
	   can-attack
	   
	   has-id))

(in-package traits)

;;;; DRAWING RELATED

(defclass has-coords ()
  ((x :accessor x
      :initarg :x)
   (y :accessor y
      :initarg :y)))

(defclass has-image ()
  ((image :accessor image
	  :initarg :image)))

(defclass can-draw (has-image has-coords) ())

(defclass has-colour ()
  ((colour :accessor colour
	   :initarg :colour)))

(defclass can-draw-colour (has-colour can-draw) ())

;;;; MOVING RELATED

(defclass can-move (has-coords) ())

;;;; LOCATION COLLISION RELATED

(defclass has-place (has-coords) ())

;;;; MAZE AND FOV RELATED

(defclass has-light-blocking ()
  ((light-blocked :accessor is-light-blocked?
		  :initarg :light-blocked)))

(defclass has-blocking ()
  ((blocked :accessor is-blocked?
	    :initarg :blocked)))

(defclass is-visitable ()
  ((visited :accessor visited?
	    :initform nil)))

;;;; OBJECT BEHAVIOUR RELATED

(defclass is-player-controlled () ())

(defclass has-ai-erratic () ())
(defclass has-ai-hunting () ())
(defclass has-ai-scaredy () ())

;;;; COMBAT RELATED

(defclass has-combat-attributes ()
  ((health   :accessor health
	     :initarg :health)
   (damages  :accessor damages
	     :initarg :damages)
   (defences :accessor defences
             :initarg :defences)))

(defclass can-attack (has-combat-attributes) ())

;;;; DIAGNOSTICS

(defclass has-id ()
  ((id :reader id
       :initarg :id)))
