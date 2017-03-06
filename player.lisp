(defpackage player
  (:use :cl :traits)
  (:export player))

(in-package player)

(defclass player (can-draw-colour
		  
		  can-move
		  has-place
		  
		  is-player-controled
		  
		  has-combat-attributes
		  can-attack
		  
		  has-id))
