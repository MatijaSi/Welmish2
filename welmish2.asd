(asdf:defsystem :welmish2
    :name "Welmish Woundikins 2: Welmish Witchings"
    :description "A funky lisp roguelike"
    :author "Matija Sirk"

    :version "0.0.1"

    :depends-on (:cl-charms :iterate)

    :serial t

    :components ((:file "io")
		 (:file "models")
		 (:file "logic")
		 (:file "welmish")))
