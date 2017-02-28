(defpackage io
  (:use :cl :iterate)
  (:export standard-window
	   new-window
	   remove-window

	   draw-char
	   draw-string

	   with-colour
	   +yellow-on-black+
       
	   clear
	   refresh
	   
	   get-char
	   
	   with-io))

(in-package io)

;;; Windows

;; Default window
(defun standard-window ()
  "Return standard window"
  charms:*standard-window*)

(defun new-window (x y h w)
  "Make a new window wide w, high h, with top left corner in x,y"
  (charms:make-window w h x y))

(defun remove-window (window)
  "Delete window"
  (charms:destroy-window window))

;;; Output

(defun draw-char (char x y &optional (window charms:*standard-window*))
  "Put char to *standard-window* at coordinates x and y"
  (charms:move-cursor window x y)
  (charms:write-char-at-cursor window char))

(defun draw-string (str x y &optional (window charms:*standard-window*))
  "Put string to *standard-window* at coordinates x and y"
  (charms:move-cursor window x y)
  (charms:write-string-at-cursor window str)
  (refresh))

;;; Coloring output (DOES NOT WORK)

(defconstant +yellow-on-black+ 1)
(charms/ll:init-pair +yellow-on-black+
		     charms/ll:COLOR_RED charms/ll:COLOR_BLUE)

(defmacro with-colour (colour &rest body)
  `(progn (charms/ll:attron (charms/ll:color-pair ,colour))
	  ,@body
	  (charms/ll:attroff (charms/ll:color-pair ,colour))))

;;; Window handling

(defun clear (&optional (window charms:*standard-window*))
  (charms:clear-window window))

(defun refresh (&optional (window charms:*standard-window*))
  (charms:refresh-window window))

;;; Input

(defun get-char (&optional (window charms:*standard-window*))
  "Get char from input"
  (charms:get-char window))

;;; Initialization and deinitialization

(defmacro with-io (&rest body)
  "Run body with initialized io, safely close it afterwards"
  `(charms:with-curses ()
     (charms:disable-echoing)
     (charms:enable-raw-input :interpret-control-characters t)
     (charms:disable-non-blocking-mode charms:*standard-window*)
     ,@body))
