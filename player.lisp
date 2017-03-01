(defpackage player
  (:use :cl)
  (:export player
	   control

	   draw-stats))

(in-package player)

(defclass player (movable:mover combat:combatant) ())

(defmethod draw-stats ((player player) &optional (offset (cons 0 0)))
  (let ((health (combat:health player))
	(raw    (combat:raw    player))
	(fire   (combat:fire   player))
	(water  (combat:water  player))
	(earth  (combat:earth  player))
	(air    (combat:air    player)))
    (io:draw-string (format nil "Health: ~A" health) 0 0 offset)
    (io:draw-string (format nil "Raw:   ~A & ~A" (car raw)   (cdr raw))   0 2 offset)
    (io:draw-string (format nil "Fire:  ~A / ~A" (car fire)  (cdr fire))  0 3 offset)
    (io:draw-string (format nil "Water: ~A / ~A" (car water) (cdr water)) 0 4 offset)
    (io:draw-string (format nil "Earth: ~A / ~A" (car earth) (cdr earth)) 0 5 offset)
    (io:draw-string (format nil "Air:   ~A / ~A" (car air)   (cdr air))   0 6 offset)))

(defmethod char->move ((player player) char level)
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
    
    (movable:move player x-mod y-mod level)))

(defmethod curse ((player player))
  (io:draw-string "Woundikins!!!" 0 1)
  (io:refresh))

(defmethod control ((player player) level)
  "Control player by keyboard"
  (let ((status :continue)
	(ch (io:get-char)))
    (case ch
      ;; Quit game
      ((#\Q #\q) (setf status :quit))
      
      ;; Display help
      ((#\H)     (setf status :help))
      
      ;; Test command for repeat -- curse command
      ((#\c)     (progn
		   (curse player)
		   (setf status :repeat)))

      ;; Test command for error -- err command
      ((#\e)      (setf status :error))

      ;; Movement
      ((#\h #\j #\l #\k #\z #\y #\u #\n #\b) (char->move player ch level)))
    status))
