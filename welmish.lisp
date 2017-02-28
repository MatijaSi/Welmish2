(defpackage welmish
  (:use :cl)
  (:export run-game))

(in-package welmish)

(defun run-game ()
  (io:with-io ()
    (logic:title-state)))
