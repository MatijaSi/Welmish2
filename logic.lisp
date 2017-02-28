(defpackage logic
  (:use :cl)
  (:export title-state
	   any-key
	   turn))

(in-package logic)

(defvar turn 0)
(defvar player (make-instance 'models:player :x 5 :y 5 :image #\@))

(defun interact-state ()
  (if (models:control player)
      (exit-state)
      (turn-increment-state)))

(defun turn-increment-state ()
  (incf turn)
  (draw-state))

(defun draw-state ()
  (io:clear)
  (models:draw player)
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
  (draw-state))

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
