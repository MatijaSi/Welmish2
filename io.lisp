(defpackage io
  (:use :cl :iterate)
  (:export draw-char
	   draw-string

	   new-offset

	   with-colour
	   +yellow-on-black+
       
	   clear
	   refresh
	   
	   get-char
	   
	   with-io))

(in-package io)

;;; Offsetted output
(defun new-offset (x y) (cons x y))

;;; Output

(defun draw-char (char x y &optional (offset (cons 0 0)))
  "Put char to *standard-window* at coordinates x and y"
  (charms:move-cursor charms:*standard-window*
		      (+ x (car offset))
		      (+ y (cdr offset)))
  (charms:write-char-at-cursor charms:*standard-window* char))

(defun draw-string (str x y &optional (offset (cons 0 0)))
  "Put string to *standard-window* at coordinates x and y"
  (charms:move-cursor charms:*standard-window*
		      (+ x (car offset))
		      (+ y (cdr offset)))
  (charms:write-string-at-cursor charms:*standard-window* str)
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

(defun clear ()
  (charms:clear-window charms:*standard-window*))

(defun refresh ()
  (charms:refresh-window charms:*standard-window*))

;;; Input

(defun get-char ()
  "Get char from input"
  (charms:get-char charms:*standard-window*))

;;; Initialization and deinitialization

(defmacro with-io (&rest body)
  "Run body with initialized io, safely close it afterwards"
  `(charms:with-curses ()
     (charms:disable-echoing)
     (charms:enable-raw-input :interpret-control-characters t)
     (charms:disable-non-blocking-mode charms:*standard-window*)
     ,@body))
