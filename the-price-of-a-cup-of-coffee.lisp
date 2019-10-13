;;;; the-price-of-a-cup-of-coffee.lisp

(in-package #:the-price-of-a-cup-of-coffee)

(def-normal-class human ()
  (walk-vec (cons 0 0))
  pos
  sheet
  (faces ((lambda () +shared-faces+)))
  (face 'facing-down)
  (frame 0)
  (next-frame-at 0))

(defun get-frame-rect (human)
  (with-slots (faces face frame) human
      (aref (funcall face faces) frame)))

(defmethod initialize-instance :after ((human human) &key)
  (with-slots (faces pos) human
    (let ((rect (get-frame-rect human)))
      (setf pos
            (sdl2:make-rect 0 0
                            (sdl2:rect-width rect)
                            (sdl2:rect-height rect))))))

(defgeneric render (sprite renderer))
(defgeneric update (thing time))

(defparameter +window-width+ 1024)
(defparameter +window-height+ 600)
(defparameter +vert-min+ 16)
(defparameter +vert-max+ (- +window-height+ 128 10))

(defvar *human-frame-pause* (/ 1000 4))
(defun set-human-fps (n)
  (setf *human-frame-pause* (/ 1000 n)))


(defmethod update ((human human) ticks)
  (with-slots (frame next-frame-at faces face walk-vec pos) human
    (setf (sdl2:rect-x pos) (mod (+ (sdl2:rect-x pos) (car walk-vec)) +window-width+))
    (setf (sdl2:rect-y pos)
          (clamp (+ (sdl2:rect-y pos) (cdr walk-vec))
                 +vert-min+ +vert-max+))
    (when (<= next-frame-at ticks)
      (setf next-frame-at (max (+ *human-frame-pause* next-frame-at) ticks))
      (setf frame (mod (1+ frame) (length (funcall face faces)))))))


(defmethod render ((human human) renderer)
  (with-slots (pos sheet faces face frame) human
    (sdl2:render-copy renderer sheet
                      :dest-rect pos
                      :source-rect (get-frame-rect human))))


(def-normal-class hero (human)
  (stress 0)
  (money 0)
  (coldness 0)
  (sick-p nil)
  (relax-rate 1))


(def-normal-class pedestrian (human)
  (comfort-rad 60)
  (react-per-sec 1)
  (anger 0.1)
  (kindness 0.02)
  (generosity 0.25)
  (vulnerability 3))



(defvar *nance*)


(defun boot-up (renderer)
  (with-surface-from-file (surf +nance-sheet-image+)
    (setf *nance*
          (make-instance 'hero
                         :sheet (sdl2:create-texture-from-surface renderer surf)))))


(defparameter +frame-delay+ (round (/ 1000 60)))

(defparameter +action-key+ :scancode-space)

(defun action-key-pressed ()
  ;; (if (eql *current-track* *looking-up-track*)
  ;;     (play-track *cold-day-track*)
  ;;     (play-track *looking-up-track*))
  (print "Action"))

(defun walking-face (dir)
  (case dir
    (:left 'walking-left)
    (:right 'walking-right)
    (:up 'walking-up)
    (:down 'walking-down)))

(defun standing-face (dir)
  (case dir
    (:left 'facing-left)
    (:right 'facing-right)
    (:up 'facing-up)
    (:down 'facing-down)))

(defun walk-hero (dir)
  (with-slots (face frame walk-vec) *nance*
    (unless (eql (walking-face dir) face)
      (setf face (walking-face dir))
      (setf frame 0)
      (case dir
        (:right (setf (car walk-vec) 6))
        (:left (setf (car walk-vec) -6))
        (:up (setf (cdr walk-vec) -6))
        (:down (setf (cdr walk-vec) 6))))))


(defun stop-hero (dir)
  (with-slots (face frame walk-vec) *nance*
    (setf face (standing-face dir))
    (setf frame 0)
    (setf (car walk-vec) 0)
    (setf (cdr walk-vec) 0)))

(defun handle-keydown (keysym)
  (let ((key (sdl2:scancode-value keysym)))
    (match-key key
      (+action-key+ (action-key-pressed))
      (:scancode-left (walk-hero :left))
      (:scancode-right (walk-hero  :right))
      (:scancode-up (walk-hero :up))
      (:scancode-down (walk-hero :down)))))


(defun handle-keyup (keysym)
  (let ((key (sdl2:scancode-value keysym)))
    (match-key key
      (:scancode-left (stop-hero :left))
      (:scancode-right (stop-hero  :right))
      (:scancode-up (stop-hero :up))
      (:scancode-down (stop-hero :down)))))


(defmethod render ((game (eql :game)) renderer)
  (sdl2:set-render-draw-color renderer 255 255 255 255)
  (sdl2:render-clear renderer)
  (render *nance* renderer)
  (sdl2:render-present renderer))

(defvar *harmony-initialized-p* nil)
(defvar *cold-day-track*)
(defvar *looking-up-track*)
(defvar *current-track*)

(defun play-track (track)
  (harmony-simple:stop *current-track*)
  (harmony-simple:resume track)
  (setf *current-track* track))


(defun main ()

  (unless *harmony-initialized-p*
    (harmony-simple:initialize)
    (setf *looking-up-track* (harmony-simple:play #p"assets/thingslookup.mp3" :music :loop t))
    (harmony-simple:stop *looking-up-track*)
    (setf *cold-day-track* (harmony-simple:play #p"assets/coldday.mp3" :music :loop t))
    (setf *current-track* *cold-day-track*)
    (setf *harmony-initialized-p* t))

  (play-track *cold-day-track*)

  (sdl2:with-init (:everything)
    (sdl2:with-window (win :w 1024 :h 600 :title "The Price Of A Cup Of Coffee" :flags '(:shown))
      (sdl2:with-renderer (renderer win :flags '(:accelerated))

        (boot-up renderer)

        (sdl2:with-event-loop (:method :poll)

          (:keydown (:keysym keysym)
                    (if (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                        (sdl2:push-event :quit)
                        (handle-keydown keysym)))

          (:keyup (:keysym keysym)  (handle-keyup keysym))

          (:idle ()
                 (render :game renderer)
                 ;; update sprites
                 (update *nance* (sdl2:get-ticks))
                 ;; update tweens
                 ;; render

                 (sdl2:delay +frame-delay+))

          (:quit ()
                 (harmony-simple:stop *current-track*)
                 (free-assets)
                 t))))))

(defun free-assets ()
  (setf (sheet *nance*) nil))

