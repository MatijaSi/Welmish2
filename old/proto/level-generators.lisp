(defpackage level-generators
  (:use :cl)
  (:export empty-level-generate
	   random-level-generate))

(in-package level-generators)

(defun simple-level-metagenerator (tile-generator width height)
  "A workhorse for simple level generators"
  (let ((level (make-instance 'level:level
			      :w width
			      :h height
			      :grid (make-array (list width height)
						:initial-element 0))))
    (level:map-level-coords #'(lambda (e x y)
				(setf (aref (level:grid level) x y)
				      (funcall tile-generator x y)))
			    level)
    level))

(defun empty-level-generate (width height)
  "A level composed of ground tiles"
  (impassable-border-generate (simple-level-metagenerator #'(lambda (x y) (level:make-ground-tile x y))
			      width
			      height)))

(defun random-level-generate (width height)
  "A level composed of ground and wall tiles randomly intermixed"
  (impassable-border-generate (simple-level-metagenerator #'(lambda (x y)
							      (if (> (random 2) 0)
								  (level:make-ground-tile x y)
								  (level:make-wall-tile x y)))
							  width
							  height)))

(defmethod impassable-border-generate ((level level:level))
  "Add walls to level border"
  (level:map-level-row\coords #'(lambda (x y) (setf (aref (level:grid level) x y)
						    (level:make-wall-tile x y)))
			      level
			      0)
  (level:map-level-row\coords
   #'(lambda (x y) (setf (aref (level:grid level) x y)
						    (level:make-wall-tile x y)))
			      level
			      (- (level:height level) 1))
  
  (level:map-level-column\coords #'(lambda (x y) (setf (aref (level:grid level) x y)
						       (level:make-wall-tile x y)))
				 level
			0)
  (level:map-level-column\coords #'(lambda (x y) (setf (aref (level:grid level) x y)
						       (level:make-wall-tile x y)))
				 level
				 (- (level:width level) 1))
  level)
