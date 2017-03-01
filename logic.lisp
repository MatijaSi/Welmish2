(defpackage logic
  (:use :cl)
  (:export title-state
	   any-key
	   turn))

(in-package logic)

;;; Test data
(defvar turn 0)
(defvar player (make-instance 'player:player :x 5 :y 5 :image #\@
			      :raw (cons 5 3) :fire (cons 5 50) :water (cons 5 50)
			      :air (cons 5 50) :earth (cons 5 50) :health 50)) 
(defvar level (level-generators:empty-level-generate 58 21))
(defvar level-offset (io:new-offset 0 2))
(defvar stats-offset (io:new-offset 59 0))

(defun interact-state ()
  (if (combat:dead? player)
      (death-state)
      (case (player:control player level)
	((:quit)     (exit-state))
	((:help)     (help-state))
	((:repeat)   (interact-state))
	((:continue) (turn-increment-state))
	(otherwise   (error-state)))))

(defun death-state ()
  (io:clear)
  (io:draw-string "You died." 0 0)
  (io:draw-string "Press any key to exit." 0 1)
  (io:refresh)
  (any-key)
  (exit-state))
  
(defun error-state ()
  (io:clear)
  (io:draw-string "Something happened and caused an error" 0 0)
  (io:draw-string "Press any key to exit" 0 1)
  (any-key)
  (exit-state))

(defun help-state ()
  (io:clear)
  (io:draw-string "Use vim keys to move"      0 0)
  (io:draw-string "Press q or Q to quit"      0 1)
  (io:draw-string "Press H for help"          0 2)
  (io:draw-string "Press any key to continue" 0 3)
  (io:refresh)
  (any-key)
  (draw-state))

(defun turn-increment-state ()
  (incf turn)
  (draw-state))

(defun draw-state ()
  (io:clear)
  (level:draw level level-offset)
  (drawable:draw player level-offset)
  (player:draw-stats player stats-offset)
  (io:draw-string (format nil "Turn: ~A" turn) 0 0)
  (io:refresh)
  (interact-state))

(defun start-state ()
  (setf turn 0)
  (draw-state))

(defun title-state ()
  (io:clear)
  (io:draw-string "Welcome to Welmish Woundikins 2: Welmish Witchings" 0 0)
  (io:draw-string "Press any key" 0 1)
  (io:refresh)
  (any-key)
  (help-state))

(defun any-key ()
  (io:get-char))

(defun exit-state ()
  (setf turn 0)
  (io:clear)
  (io:draw-string "Thanks for playing." 0 0)
  (io:draw-string "Press any key to exit." 0 1)
  (any-key)
  (io:clear)
  (io:refresh))
