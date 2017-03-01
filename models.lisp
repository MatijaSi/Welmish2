(defpackage models
  (:use :cl)
  (:export tile
	   draw

	   level
	   level-tile
	   make-ground-tile
	   make-wall-tile
	   empty-level-generate
	   random-level-generate
	   map-level
	   map-level-coords
	   get-tile

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

(defmethod draw ((tile tile) &optional (offset (cons 0 0)))
  "Draw tile to window"
  (io:draw-char (image tile) (x tile) (y tile) offset))

(defclass level-tile (tile)
  ((blocked? :reader   blocked?
	     :initarg :blocked?)))

;; A game map class
(defclass level ()
  ((height :reader   height
	   :initarg :h)
   (width  :reader   width
	   :initarg :w)
   (grid   :accessor grid
	   :initarg :grid)))

(defun make-ground-tile (x y)
  (make-instance 'level-tile :x x :y y :blocked? nil :image #\.))

(defun make-wall-tile (x y)
  (make-instance 'level-tile :x x :y y :blocked? t :image #\#))

(defun simple-level-metagenerator (tile-generator width height)
  "A workhorse for simple level generators"
  (let ((level (make-instance 'level
			      :w width
			      :h height
			      :grid (make-array (list width height)
						:initial-element 0))))
    (map-level-coords #'(lambda (e x y)
			  (setf (aref (grid level) x y)
				(funcall tile-generator x y)))
		      level)
    level))

(defun empty-level-generate (width height)
  "A level composed of ground tiles"
  (simple-level-metagenerator #'(lambda (x y) (make-ground-tile x y))
			      width
			      height))

(defun random-level-generate (width height)
  "A level composed of ground and wall tiles randomly intermixed"
  (impassable-border-generate (simple-level-metagenerator #'(lambda (x y)
							      (if (> (random 2) 0)
								  (make-ground-tile x y)
								  (make-wall-tile x y)))
							  width
							  height)))

(defmethod impassable-border-generate ((level level))
  "Add walls to level border"
  (map-level-row\coords #'(lambda (x y) (setf (aref (grid level) x y)
					      (make-wall-tile x y)))
			level
			0)
  (map-level-row\coords #'(lambda (x y) (setf (aref (grid level) x y)
					      (make-wall-tile x y)))
			level
			(- (height level) 1))
  (map-level-column\coords #'(lambda (x y) (setf (aref (grid level) x y)
						 (make-wall-tile x y)))
			   level
			0)
  (map-level-column\coords #'(lambda (x y) (setf (aref (grid level) x y)
						 (make-wall-tile x y)))
			   level
			   (- (width level) 1))
  level)

(defmethod map-level (fun (level level))
  "Apply fun to all elements of level"
  (do ((x 0 (+ x 1)))
      ((= x (width level)) level)
    (do ((y 0 (+ y 1)))
	((= y (height level)) 'done)
      (funcall fun (aref (grid level) x y)))))

(defmethod map-level-row (fun (level level) y)
  "Apply fun to all elements of row y"
  (do ((x 0 (+ x 1)))
      ((= x (width level)) level)
    (funcall fun (get-tile level x y))))

(defmethod map-level-row\coords (fun (level level) y)
  "Apply fun to all coords of row y"
  (do ((x 0 (+ x 1)))
      ((= x (width level)) level)
    (funcall fun x y)))

(defmethod map-level-column (fun (level level) x)
  "Apply fun to all elements of column x"
  (do ((y 0 (+ y 1)))
      ((= y (height level)) level)
    (funcall fun (get-tile level x y))))

(defmethod map-level-column\coords (fun (level level) x)
  "Apply fun to all coords of column x"
  (do ((y 0 (+ y 1)))
      ((= y (height level)) level)
    (funcall fun x y)))

(defmethod map-level-coords (fun (level level))
  "Apply fun to all elements of level, pass it their coords too"
  (do ((x 0 (+ x 1)))
      ((= x (width level)) level)
    (do ((y 0 (+ y 1)))
	((= y (height level)) 'done)
      (funcall fun (aref (grid level) x y) x y))))

(defmethod get-tile ((level level) x y)
  (aref (grid level) x y))

(defmethod draw ((level level) &optional (offset (cons 0 0)))
  (map-level #'(lambda (tile) (draw tile offset)) level))

(defclass mover (tile)
  ((x :accessor x
      :initarg :x)
   (y :accessor y
      :initarg :y)))

(defmethod move ((mover mover) x-mod y-mod level)
  "Move mover for x-mod in x and for y-mod in y"
  (let ((new-x (+ (x mover) x-mod))
	(new-y (+ (y mover) y-mod)))
    (unless (blocked? (get-tile level new-x new-y))
      (setf (x mover) new-x)
      (setf (y mover) new-y))))

(defclass player (mover) ())

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
    
    (move player x-mod y-mod level)))

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
