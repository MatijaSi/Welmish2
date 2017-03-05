;;;;; Methods for working with trais

(in-package traits)

(defmethod draw ((obj can-draw) &optional (offset (cons 0 0)))
  "Draw object with optional offset"
  (io:draw-char (image obj)
		(+ (x obj) (car offset))
		(+ (y obj) (cdr offset))))

(defmethod draw ((obj can-draw-colour) &optional (offset (cons 0 0)))
  (with-colour (colour obj)
    (io:draw-char (image obj)
		  (+ (x obj) (car offset))
		  (+ (y obj) (cdr offset)))))

(defmethod move ((obj can-move) mod-x mod-y)
  "Move object by mod-x and mod-y"
  (incf (x obj) mod-x)
  (incf (y obj) mod-y))

(defmethod is-here? ((obj has-place) x y)
  "True if object occupies coordinates x and y, else false"
  (and (= (x obj) x)
       (= (y obj) y)))

(defmethod attack ((attacker can-attack) (defender has-combat-attributes))
  (labels ((formula (el-atk el-def) ; formula for elemental damage
	     (* (car el-atk) (/ (- 100 (cdr el-def)) 100))))

    ;; individual damage calculations
    (let ((fire  (formula (fire attacker)  (fire defender)))
	  (water (formula (water attacker) (water defender)))
	  (air   (formula (air attacker)   (air defender)))
	  (earth (formula (earth attacker) (earth defender)))
	  
	  (raw (- (car (raw attacker))
		  (cdr (raw defender)))))
      
      ;; raw damage can't be under 0
      (unless (> raw-dmg 0)
	(setf raw-dmg 0))

      ;; calculate total damage
      (let ((damage (+ raw fire water earth air)))
	(decf (health defender) damage)

	;; battle report
	(io:draw-format 0 0 "~A hit ~A for ~A"
			(identify attacker)
			(identify defender)
			damage)))))

(defmethod dead? ((object has-combat-attributes))
  (< (health object) 0))

(defmethod identify (obj)
  "Unknown obj")

(defmethod identify ((obj has-id))
  (id obj))
