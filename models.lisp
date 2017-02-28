(defpackage models
  (:use :cl)
  (:export tile
	   draw

	   mover
	   move

	   player
	   control))

(in-package models)

(defclass tile ()
  ((x     :reader   x
          :initarg :x)
   (y     :reader   y
          :initarg :y)
   (image :reader   image
	  :initarg :image)))

(defmethod draw ((tile tile))
  "Draw tile to *standard-window*"
  (io:draw-char (image tile) (x tile) (y tile)))

(defclass mover (tile)
  ((x :accessor x
      :initarg :x)
   (y :accessor y
      :initarg :y)))

(defmethod move ((mover mover) x-mod y-mod)
  "Move mover for x-mod in x and for y-mod in y"
  (incf (x mover) x-mod)
  (incf (y mover) y-mod))

(defclass player (mover) ())

(defmethod control ((player player))
  "Control player by keyboard"
  (let ((quit? nil)
	(x-mod 0)
	(y-mod 0))
    (case (io:get-char)
      ;; Quit game
      ((#\Q #\q) (setf quit? t))

      ;; Movement keys
      ((#\h)     (decf x-mod))
      ((#\j)     (incf y-mod))
      ((#\k)     (decf y-mod))
      ((#\l)     (incf x-mod))

      ;; Diagonal movement
      ((#\z #\y) (progn (decf x-mod)
			(decf y-mod)))
      ((#\b)     (progn (decf x-mod)
			(incf y-mod)))
      ((#\u)     (progn (incf x-mod)
			(decf y-mod)))
      ((#\n)     (progn (incf x-mod)
			(incf y-mod))))
    (move player x-mod y-mod)
    quit?))
