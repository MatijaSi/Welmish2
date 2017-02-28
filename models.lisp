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

(defclass map-tile ((tile))
  ((blocked? :reader blocked?
	     :initarg :blocked?)))

(defmethod draw ((tile tile))
  "Draw tile to *standard-window*"
  (io:draw-char (image tile) (x tile) (y tile)))

(defclass map ()
  ((grid    :initarg :grid
	    :accessor grid)
   (rows    :initarg :rows
	    :reader   rows)
   (columns :initarg :columns
	    :reader   columns)))

(defun map-new (rows columns)
  "Return rows x columns map"
  (let* ((map (make-instance 'map :rows rows :columns columns))
	 (array (make-array (list rows columns) :initial-element 0)))
    (setf (grid map) array)
    (grid-map-coordinated #'(lambda (a row column)
			      (make-instance 'map-tile
					     :x row
					     :y column
					     :blocked? nil)) grid)))

(defun map-make (rows columns array)
  "Return map with grid equal to array"
  (make-instance 'map :rows rows :columns columns :grid array))

(defmethod map-map (fun (map map))
  "Apply fun to all elements of map and collect them in new map"
  (let* ((rows (rows map))
	 (columns (columns map))
	 (result-array (make-array (list rows columns)))
	 (array (grid map)))
    (do ((row 0 (+ row 1)))
	((= row rows) (map-make rows columns result-array))
      (do ((column 0 (+ column 1)))
	  ((= column columns) nil)
	(setf (aref result-array row column)
	      (funcall fun (aref array row column)))))))

(defmethod map-map-coordinated (fun (map map))
  "Apply fun to all elements of map and collect them in new map.
Fun must take three arguments, first one element, second row and third column."
  (let* ((rows (rows map))
	 (columns (columns map))
	 (result-array (make-array (list rows columns)))
	 (array (grid map)))
    (do ((row 0 (+ row 1)))
	((= row rows) (map-make rows columns result-array))
      (do ((column 0 (+ column 1)))
	  ((= column columns) nil)
	(setf (aref result-array row column)
	      (funcall fun (aref array row column) row column))))))

(defmethod map-get (x y (map map))
  "Get x,y cell of grid"
  (aref (grid map) x y))

(defmethod map-row (x (map map))
  "Return all cells with x"
  (let ((limit (columns map))
	(list '()))
    (do ((y 0 (+ y 1)))
	((= y limit) (reverse list))
      (push (map-get x y map) list))))

(defmethod map-rows ((map map))
  "Return list of rows"
  (let ((limit (rows map))
	(result '()))
    (do ((x 0 (+ x 1)))
	((= x limit) (reverse result))
      (push (map-row x map) result))))

(defmethod map-column (y (map map))
  "Return all cells with y"
  (let ((limit (rows map))
	(list '()))
    (do ((x 0 (+ x 1)))
	((= x limit) list)
      (push (map-get x y map) list))))

(defmethod map-all ((map map))
  "Return all cells of map"
  (let ((rows (rows map))
	(columns (columns map))
	(list '()))
    (do ((x 0 (+ x 1)))
	((= x rows) list)
      (do ((y 0 (+ y 1)))
	  ((= y columns) nil)
	(push (map-get x y map) list)))))

(defmethod map-random ((map map))
  "Return random cell from map"
  (let ((y (random (columns map)))
	(x (random (rows map))))
    (map-get x y grid)))

(defmethod map-surface ((map map))
  "Return rows * columns of map"
  (* (rows map) (columns map)))

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

(defmethod curse ((player player))
  (io:draw-string "Woundikins!!!" 0 1)
  (io:refresh))

(defmethod control ((player player))
  "Control player by keyboard"
  (let ((status :continue)
	(x-mod 0)
	(y-mod 0))
    (case (io:get-char)
      ;; Quit game
      ((#\Q #\q) (setf status :quit))

      ;; Display help
      ((#\H)     (setf status :help))

      ;; Test command for repeat -- curse command
      ((#\c)     (progn
		   (curse player)
		   (setf status :repeat)))
	
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
    status))
