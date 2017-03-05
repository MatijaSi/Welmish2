(asdf:defsystem :welmish2
    :name "Welmish Woundikins 2: Welmish Witchings"
    :description "A funky lisp roguelike"
    :author "Matija Sirk"

    :version "0.0.2"

    :depends-on (:croatoan)

    :serial t

    :components ((:file "io")
		 (:file "traits")
		 (:file "traits-methods")))
