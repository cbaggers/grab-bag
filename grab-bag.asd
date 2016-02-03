;;;; grab-bag.asd

(asdf:defsystem #:grab-bag
  :description "A simple but somewhat optimized pool for objects."
  :author "Chris Bagley (Baggers) <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :components ((:file "package")
               (:file "typed-bag")))
