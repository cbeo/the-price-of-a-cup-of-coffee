;;;; the-price-of-a-cup-of-coffee.asd

(asdf:defsystem #:the-price-of-a-cup-of-coffee
  :description "Just a cold day on a busy street."
  :author "<thegoofist@protonmail.com>"
  :license  "GPL3"
  :version "0.0.1"
  :serial t
  :depends-on (#:animise #:sdl2 #:sdl2-image #:harmony)
  :components ((:file "package")
               (:file "the-price-of-a-cup-of-coffee")))
