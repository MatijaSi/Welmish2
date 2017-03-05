(defpackage io
  (:use :cl :ncurses)
  (:export :with-io

	   :get-char

	   :draw-char
	   :draw-string
	   :draw-format

	   :clear
	   :refresh
	   
	   :with-colour

	   :+black+
	   :+white+
	   :+yellow+
	   :+cyan+
	   :+green+
	   :+blue+
	   :+red+
	   :+magenta+))

(in-package io)

;;; Initialization and finalization

(defun initialize ()
  "Prepare screen for drawing. 
It must be evald before other io functions.
Use with-io macro to guarantee that.
Takes no arguments, returns no (useful) output."
  (%initscr)
  (%noecho)
  (%start-color)
  (prepare-colours)
  (clear))

(defun finalize ()
  "Clean the screen after drawing and close it.
Call it only after other io functions.
Use with-io macro to guarantee that.
Takes no arguments, returns no (useful) output."
  (clear)
  (%endwin))

(defmacro with-io (&rest body)
  "Run arguments with initialized screen.
After they are evald screen is cleaned up and closed.
Equivalent to running initialize before arguments and finalize after it.
Takes any number of arguments, has no (useful) outputs."
  `(progn (initialize)
	  ,@body
	  (finalize)))
;;; Input

(defun get-char ()
  "Return char from screen input.
Waits for user input.
Takes no arguments, returns character."
  (code-char (%getch)))

;;; Output

(defun draw-string (string x y)
  "Draw string to screen.
Arguments are string, x and y coordinates.
String will be drawn as it is, beginning at x,y coords.
Returns no (useful) output."
  (%mvaddstr y x string))

(defun draw-char (char x y)
  "Draw char to screen.
Arguments are char, x and y coordinates.
Char will be drawn as it is on x,y coords.
Returns no (useful) output."
  (%mvaddch y x (char-code char)))

(defun draw-format (x y &rest format-args)
  "Draw formated string to screen.
Arguments are x,y and format-args.
String returned from (apply #'format (cons nil format-args)) will
be drawn to screen, beginning at x,y coords.
Returns no (useful) output"
  (%mvaddstr y x (apply #'format (cons nil format-args))))

;;; Misc Output

(defun clear ()
  "Clears screen.
Takes no arguments, returns no (useful) output."
  (%clear))

(defun refresh ()
  "Flushes changes to screen.
Takes no arguments, returns no (useful) output:"
  (%refresh))

;;; Colors

(defconstant +black+   1)
(defconstant +red+     2)
(defconstant +green+   3)
(defconstant +yellow+  4)
(defconstant +blue+    5)
(defconstant +magenta+ 6)
(defconstant +cyan+    7)
(defconstant +white+   0)

(defun prepare-colours ()
  "Initialize colour pairs for drawing on screen.
Don't use directly, it's contained in initialize."
 (mapcar #'(lambda (pair color)
      (%init-pair pair color %COLOR_BLACK))
  (list +black+ +red+     +green+ +yellow+
	+blue+  +magenta+ +cyan+  +white+)
  (list %COLOR_BLACK %COLOR_RED     %COLOR_GREEN %COLOR_YELLOW
	%COLOR_BLUE  %COLOR_MAGENTA %COLOR_CYAN  %COLOR_WHITE)))

(defun colour-set (pair)
  "Use colour described by pair in following io operations.
Takes colour pair for argument, has no (useful) output."
  (%attron (%color-pair pair)))

(defun colour-unset (pair)
  "Stop using colour described by pair for io operations
Takes colour pair, returns nothing useful."
  (%attroff (%color-pair pair)))

(defmacro with-colour (colour &rest body)
  "Takes at least one argument, has no useful output.
First argument is colour, which will be used with io operations in
other arguments."
  `(progn (colour-set ,colour)
	  ,@body
	  (colour-unset ,colour)))
