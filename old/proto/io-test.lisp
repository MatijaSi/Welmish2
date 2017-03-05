(in-package io)

(with-io (draw-string "Hello world!" 1 2)
  (draw-char #\a 0 0)
  (refresh)
  (clear)
  (refresh)

  (with-colour +red+
    (draw-format 4 5 "Hello! on x=~A and y=~A" 4 5)
    (draw-char (get-char) 4 3))

  (refresh)

  (with-colour +yellow+
    (draw-char #\@ 7 7))

  (refresh)
  (get-char))

