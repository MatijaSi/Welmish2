(defpackage player
  (:use :cl :traits)
  (:export :player
	   :control
	   
	   :draw-stats))

(defclass player (can-draw-colour
		  
		  can-move
		  has-place
		  
		  is-player-controlled
		  
		  has-combat-attributes
		  can-attack
		  
		  has-id) ())

(defmethod char->move ((player player))
  "Call move on player by appropriate x-mod, y-mod"
  (let ((x-mod 0)
	(y-mod 0))
    
    (case char
      ((#\h)     (decf x-mod)) ; left
      ((#\j)     (incf y-mod)) ; down
      ((#\k)     (decf y-mod)) ; up
      ((#\l)     (incf x-mod)) ; right
      
      
      ((#\z #\y) (progn (decf x-mod) ;   left
			(decf y-mod))) ; up
      
      ((#\b)     (progn (decf x-mod) ;   left
			(incf y-mod))) ; down
      
      ((#\u)     (progn (incf x-mod)   ; right
			(decf y-mod))) ; up
      
      ((#\n)     (progn (incf x-mod)   ; right
			(incf y-mod)))); up
    
    (move player x-mod y-mod)))

(defmethod control ((actor is-player-controlled)))
