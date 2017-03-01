(defpackage io
  (use :cl))

;;; Initialization and finalization

(defun open-window ()
  (make-instance 'croatoan:screen
		 :enable-colors t :input-echoing nil
		 :input-blocking t :enable-fkeys t
		 :cursor-visibility nil))

(defun close-window ()
  (croatoan:end-screen))
;;; Input

(defun get-char (window)
  (code-char (croatoan:get-char window)))

;;; Output

(defun draw-string (window string x y)
  (croatoan:add-string window string :x x :y y))

(defun draw-char (window char x y)
  (croatoan:add-char window (char-code char) :x x :y y))

;;; Misc Output

(defun clear (window)
  (croatoan:clear window))

(defun refresh (window)
  (croatoan:refresh window))

;;; Colors

(defun set-color (window fg bg)
  (setf (croatoan:.color-pair window) (list fg bg)))

