;;;; assets.lisp

(in-package #:the-price-of-a-cup-of-coffee)

;;; Utility Functions
(defun find-tile-rect (tile-defs name)
  "Return the rect for the tile with NAME in TILE-DEFS list."
  (let-when (tl (find name tile-defs
                      :key (lambda (tl) (getf tl :name))
                      :test #'string-equal))
    (sdl2:make-rect (getf tl :x)
                    (getf tl :y)
                    (getf tl :width)
                    (getf tl :height))))

(defun select-tile-rects (tile-defs &rest names)
  (map 'vector ($ #'find-tile-rect tile-defs _) names))

(defstruct (sprite-faces (:conc-name ""))
  facing-down walking-down facing-up walking-up
  facing-left walking-left facing-right walking-right)

(defun create-sprite-faces (defs)
  (make-sprite-faces
   :facing-down (select-tile-rects defs "front")
   :walking-down (select-tile-rects defs "walkforward1" "front" "walkforward2" "front")
   :facing-up (select-tile-rects defs "back")
   :walking-up (select-tile-rects defs  "walkback1" "back" "walkback2" "back")
   :facing-left (select-tile-rects defs "leftprofile")
   :walking-left (select-tile-rects defs "walkleft1" "leftprofile" "walkleft2" "leftprofile")
   :facing-right (select-tile-rects defs "rightprofile")
   :walking-right (select-tile-rects defs "walkright1" "rightprofile" "walkright1" "rightprofile")))

(defparameter +tile-defs+
  '((:NAME "Back" :X 256 :Y 128  :WIDTH 64 :HEIGHT 128)
    (:NAME "Front" :X 320 :Y 0  :WIDTH 64 :HEIGHT 128)
    (:NAME "LeftProfile" :X 192 :Y 128  :WIDTH 64 :HEIGHT 128)
    (:NAME "RightProfile" :X 256 :Y 0  :WIDTH 64 :HEIGHT 128)
    (:NAME "WalkBack1" :X 64 :Y 128  :WIDTH 64 :HEIGHT 128)
    (:NAME "WalkBack2" :X 128 :Y 128  :WIDTH 64 :HEIGHT 128)
    (:NAME "WalkForward1" :X 0 :Y 256  :WIDTH 64 :HEIGHT 128)
    (:NAME "WalkForward2" :X 192 :Y 0  :WIDTH 64 :HEIGHT 128)
    (:NAME "WalkLeft1" :X 128 :Y 0  :WIDTH 64 :HEIGHT 128)
    (:NAME "WalkLeft2" :X 0 :Y 128  :WIDTH 64 :HEIGHT 128)
    (:NAME "WalkRight1" :X 64 :Y 0  :WIDTH 64 :HEIGHT 128)
    (:NAME "WalkRight2" :X 0 :Y 0  :WIDTH 64 :HEIGHT 128)))

(defparameter +shared-faces+
  (create-sprite-faces +tile-defs+))

(defparameter +nance-sheet-image+ "assets/Nance.png")
(defparameter +suit-sheet-image+ "assets/Suit.png")

(defvar *nance-texture*)
(defvar *suit-texture*)





