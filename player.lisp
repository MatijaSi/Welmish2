(defpackage player
  (:use :cl)
  (:export player
	   control))

(in-package player)

(defclass player (movable:mover) ())

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
