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


(defparameter +emoji-defs+
'((:NAME "alarmed-question" :X 288 :Y 216  :WIDTH 72 :HEIGHT 72)
 (:NAME "alarmed" :X 288 :Y 144  :WIDTH 72 :HEIGHT 72)
 (:NAME "angry" :X 288 :Y 288
  :WIDTH 72 :HEIGHT 72)
 (:NAME "asshole" :X 360 :Y 0  :WIDTH 72 :HEIGHT 72)
 (:NAME "breakdown" :X 72 :Y 288  :WIDTH 72 :HEIGHT 72)
 (:NAME "chance" :X 144 :Y 288  :WIDTH 72 :HEIGHT 72)
 (:NAME "coffee" :X 216 :Y 288  :WIDTH 72 :HEIGHT 72)
 (:NAME "cold" :X 0 :Y 360
  :WIDTH 72 :HEIGHT 72)
 (:NAME "death" :X 216 :Y 72
  :WIDTH 72 :HEIGHT 72)
 (:NAME "dollars" :X 216 :Y 144  :WIDTH 72 :HEIGHT 72)
 (:NAME "food1" :X 216 :Y 216
  :WIDTH 72 :HEIGHT 72)
 (:NAME "food2" :X 288 :Y 0
  :WIDTH 72 :HEIGHT 72)
 (:NAME "food3" :X 72 :Y 216
  :WIDTH 72 :HEIGHT 72)
 (:NAME "food4" :X 144 :Y 216
  :WIDTH 72 :HEIGHT 72)
 (:NAME "food5" :X 0 :Y 288
  :WIDTH 72 :HEIGHT 72)
 (:NAME "heart1" :X 144 :Y 72
  :WIDTH 72 :HEIGHT 72)
 (:NAME "heart2" :X 144 :Y 144  :WIDTH 72 :HEIGHT 72)
 (:NAME "heart3" :X 216 :Y 0
  :WIDTH 72 :HEIGHT 72)
 (:NAME "incapacitated" :X 72 :Y 144  :WIDTH 72 :HEIGHT 72)
 (:NAME "nauseated" :X 0 :Y 216  :WIDTH 72 :HEIGHT 72)
 (:NAME "neutral" :X 72 :Y 72  :WIDTH 72 :HEIGHT 72)
 (:NAME "relaxed" :X 144 :Y 0  :WIDTH 72 :HEIGHT 72)
 (:NAME "sick" :X 0 :Y 144
  :WIDTH 72 :HEIGHT 72)
 (:NAME "sorry-no" :X 72 :Y 0  :WIDTH 72 :HEIGHT 72)
 (:NAME "stressed" :X 0 :Y 72  :WIDTH 72 :HEIGHT 72)
 (:NAME "very-angry" :X 0 :Y 0  :WIDTH 72 :HEIGHT 72)))


(defparameter +shared-faces+
  (create-sprite-faces +tile-defs+))



(defparameter +nance-sheet-image+ #P"assets/Nance.png")
(defparameter +suit-sheet-image+ #P"assets/Suit.png")
(defparameter +nomry-sheet-image+ #P"assets/Normy.png")
(defparameter +kid-sheet-image+ #P"assets/KidSheet.png")
(defparameter +punker-sheet-image+ #P"assets/Punker.png")
(defparameter +things-look-up-track-path+ #P"assets/thingslookup.mp3")
(defparameter +cold-day-track-path+ #P"assets/coldday.mp3")
(defparameter +emoji-sheet-image+ #P"assets/emoji.png")
(defparameter +sliding-door-image+ #P"assets/sliding-door.png")
(defparameter +backdrop-image+ #P"assets/backdrop.png")
(defparameter +speechbubble-image+ #P"assets/Speechbubble.png")
(defparameter +cop1-sheet-image+ #P"assets/Cop1.png")
(defparameter +cop2-sheet-image+ #P"assets/Cop2.png")
(defparameter +title-card-image+ #P"assets/TitleCard.png")

(defvar *nance-texture*)
(defvar *suit-texture*)
(defvar *normy-texture*)
(defvar *kid-texture*)
(defvar *punker-texture*)
(defvar *cop1-texture*)
(defvar *cop2-texture*)
(defvar *expression-texture*)
(defvar *backdrop-texture*)
(defvar *sliding-door-texture*)
(defvar *speech-bubble-texture*)
(defvar *title-card-texture*)

(defparameter +sliding-door-open-x+ 800)
(defparameter +sliding-door-closed-x+ 868)
(defvar *sliding-door-position*
  (sdl2:make-rect +sliding-door-closed-x+ 8 104 138))

(defvar *harmony-initialized-p* nil)
(defvar *cold-day-track*)
(defvar *looking-up-track*)
(defvar *current-track*)

(defvar *cop-animation-faces*
  (let* ((f1 (sdl2:make-rect 0 0 64 128))
         (f2 (sdl2:make-rect 64 0 64 128))
         (v (make-array 2 :initial-contents (list f1 f2))))
    (make-sprite-faces
     :facing-down v :facing-up v :facing-left v :facing-right v
     :walking-down v :walking-up v :walking-left v :walking-right v)))

(defun make-texture-from-file (renderer filepath)
  (with-surface-from-file (surf filepath)
    (sdl2:create-texture-from-surface renderer surf)))


(defun boot-and-show-title (renderer)
  (setf *title-card-texture* (make-texture-from-file renderer +title-card-image+))
  (sdl2:render-copy renderer *title-card-texture*)
  (sdl2:render-present renderer))


(defun boot-up-assets (renderer)
  (setf *nance-texture* (make-texture-from-file renderer  +nance-sheet-image+))
  (setf *suit-texture* (make-texture-from-file renderer  +suit-sheet-image+))
  (setf *normy-texture* (make-texture-from-file renderer  +nomry-sheet-image+))
  (setf *kid-texture* (make-texture-from-file renderer +kid-sheet-image+))
  (setf *punker-texture* (make-texture-from-file renderer +punker-sheet-image+))
  (setf *expression-texture* (make-texture-from-file renderer  +emoji-sheet-image+))
  (setf *sliding-door-texture* (make-texture-from-file renderer  +sliding-door-image+))
  (setf *backdrop-texture* (make-texture-from-file renderer  +backdrop-image+))
  (setf *speech-bubble-texture* (make-texture-from-file renderer +speechbubble-image+))
  (setf *cop1-texture* (make-texture-from-file renderer +cop1-sheet-image+))
  (setf *cop2-texture* (make-texture-from-file renderer +cop2-sheet-image+))

  (unless *harmony-initialized-p*
    (harmony-simple:initialize)
    (setf *looking-up-track* (harmony-simple:play +things-look-up-track-path+ :music :loop t))
    (harmony-simple:stop *looking-up-track*)
    (setf *cold-day-track* (harmony-simple:play +cold-day-track-path+ :music :loop t))
    (setf *current-track* *cold-day-track*)
    (harmony-simple:stop *cold-day-track*)
    (setf *harmony-initialized-p* t)))

(defun free-assets ()
  (harmony-simple:stop *current-track*)
  (sdl2:destroy-texture *expression-texture*)
  (sdl2:destroy-texture *nance-texture*)
  (sdl2:destroy-texture *normy-texture*)
  (sdl2:destroy-texture *suit-texture*))

(defvar *expression-cache* (make-hash-table :test 'equal))

(defun get-expression (expression)
  "Returns a rect used as the render source in a call to RENDER-COPY"
  (let ((exp (gethash expression *expression-cache*)))
    (unless exp
      (setf exp (find-tile-rect +emoji-defs+ expression))
      (setf (gethash expression *expression-cache*) exp))
    exp))

