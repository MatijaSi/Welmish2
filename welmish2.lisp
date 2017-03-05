(defpackage welmish2
  (:use :cl)
  (:export :run-game))

(in-package welmish2)

(defun run-game ()
  "Run game."
  (io:with-io
      (logic:init-state)))
