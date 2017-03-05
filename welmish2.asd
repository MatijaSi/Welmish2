(asdf:defsystem :welmish2
    :name "Welmish Woundikins 2: Welmish Witchings"
    :description "A funky lisp roguelike"
    :author "Matija Sirk"

    :version "0.0.3"

    :depends-on (:ncurses)

    :serial t

    :components ((:file "io")
		 (:file "logic")
		 (:file "welmish2")))
