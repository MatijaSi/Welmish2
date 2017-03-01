(defpackage logic
  (:use :cl)
  (:export title-state
	   any-key
	   turn))

(in-package logic)

;;; Test data
(defvar turn 0)
(defvar player (make-instance 'models:player :x 5 :y 5 :image #\@))
(defvar level (models:random-level-generate 40 15))
(defvar level-offset (io:new-offset 0 2))

(defun interact-state ()
  (case (models:control player level)
    ((:quit)     (exit-state))
    ((:help)     (help-state))
    ((:repeat)   (interact-state))
    ((:continue) (turn-increment-state))
    (otherwise   (error-state))))

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
  (models:draw level level-offset)
  (models:draw player level-offset)
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
