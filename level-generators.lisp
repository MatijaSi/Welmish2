(defpackage level-generators
  (:use :cl)
  (:export empty-level-generate
	   random-level-generate))

(in-package level-generators)

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
