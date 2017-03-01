(defpackage monsters
  (:use :cl)
  (:export monster
	   control))

(in-package monsters)

(defclass monster (movable:mover combat:combatant) ())

(defmethod control ((monster monster) level player monsters)
    (movable:move monster
		  (- (random 3) 1)
		  (- (random 3) 1)
		  level
		  player
		  monsters))
