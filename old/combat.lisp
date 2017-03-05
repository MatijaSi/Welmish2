(defpackage combat
  (:use :cl)
  (:export combatant
	   attack
	   dead?

	   health
	   
	   fire
	   water
	   earth
	   air
	   raw))

(in-package combat)

(defvar report-offset (io:new-offset 0 23)) ;; refactor this shit

(defclass combatant ()
  ((health :initarg :health
	   :accessor health)
   
   ;;; format: (cons dmg res)
   
   ;; Raw armour and damage 
   (raw    :initarg :raw
	   :accessor raw)
   
   ;; Elemenental damage, resistances
   (fire  :initarg :fire
	  :accessor fire)
   (water :initarg :water
	  :accessor water)
   (earth :initarg :earth
	 :accessor earth)
   (air   :initarg :air
	 :accessor air)))

(defmethod attack ((attacker combatant) (defender combatant))
  (labels ((combat-formula (el-atk el-def)
	     (* (car el-atk) (/ (- 100 (cdr el-def)) 100))))
    
    (let ((fire-dmg  (combat-formula (fire attacker)      (fire defender)))
	  (water-dmg (combat-formula (water attacker)     (water defender)))
	  (earth-dmg (combat-formula (earth attacker)     (earth defender)))
	  (air-dmg   (combat-formula (air attacker)       (air defender)))
	  (raw-dmg   (-              (car (raw attacker)) (cdr (raw defender)))))

      (unless (> raw-dmg 0)
	(setf raw-dmg 0))

      (decf (health defender) (+ raw-dmg fire-dmg water-dmg earth-dmg air-dmg))

      (io:draw-string (format nil "A hit b for ~A"
			      (+ raw-dmg fire-dmg water-dmg earth-dmg air-dmg))
		      0 0 report-offset)
      (io:refresh))))

(defmethod dead? ((combatant combatant))
  (< (health combatant) 0))
	   
