(defpackage logic
  (:use :cl)
  (:export :init-state))

;;;; In this file "game loop" is defined.
;;;; It's not loop per se, but it's portrayed as state machine
;;;; States are portrayed as simple functions, where last action is
;;;; always state transition, except in quit-state and in error-state
;;;; First state is init-state
;;;; Game usually exists through quit-state
;;;; If error occurs game exists through error-state
;;;; game loop as state machine was inspired by silt2
(in-package logic)

(defun init-state ()
  "First state, give player a nice welcome message,
then go to help-state"
  (io:with-tabula-rasa
      (io:draw-strings 0 0
		       "Welcome to Welmish Woundikins 2: Welmish Witchings"
		       "Press any key to begin."))
  (io:any-key)
  (help-state))

(defun help-state ()
  "Give player information about game,
then go to draw-state"
  (io:with-tabula-rasa
      (io:draw-strings 0 0
		       "Use hjkl to move"
		       "Use y or z and unb to diagonally move"
		       "Press q or Q to quit"
		       "Press H for help"
		       ""
		       "Press any key to continue"))
  (io:any-key)
  (draw-state))

(defun draw-state ()
  "Just a placeholder for now"
  (interact-state))

(defun interact-state ()
  "Placeholder too"
  (case (io:get-char)
    ((#\H) (help-state))
    ((#\q #\Q) (quit-state))
    (otherwise (draw-state))))

(defun quit-state ()
  "Give player a nice message and quit game"
  (io:with-tabula-rasa
      (io:draw-strings 0 0
		       "Thanks for playing!"
		       "Press any key"))
  (any-key))