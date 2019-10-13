;;;; the-price-of-a-cup-of-coffee.lisp

(in-package #:the-price-of-a-cup-of-coffee)

(def-normal-class human ()
  (walk-vec (cons 0 0))
  (walk-speed 6)
  diag-walk-speed
  pos
  sheet
  (faces ((lambda () +shared-faces+)))
  (face 'facing-down)
  (frame 0)
  (next-frame-at 0))

(defun walking-p (human)
  (not (standing-p human)))

(defun standing-p (human)
  (and (zerop (car (walk-vec human)))
       (zerop (cdr (walk-vec human)))))


(defmethod (setf walk-speed) :after (newval (human human))
  (setf (diag-walk-speed human)
        (floor (sqrt (* 0.5 (* newval newval))))))

(defun get-frame-rect (human)
  (with-slots (faces face frame) human
      (aref (funcall face faces) frame)))

(defmethod initialize-instance :after ((human human) &key)
  (setf (walk-speed human) 6)
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
    (incf (sdl2:rect-x pos) (car walk-vec))
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

(defmethod update :after ((hero hero) ticks)
  (with-slots (pos) hero
    (setf (sdl2:rect-x pos)
          (mod (sdl2:rect-x pos) +window-width+))))


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
    (setf *nance-texture*  (sdl2:create-texture-from-surface renderer surf)))

  (with-surface-from-file (surf +suit-sheet-image+)
    (setf *suit-texture* (sdl2:create-texture-from-surface renderer surf)))

  (setf *nance* (make-instance 'hero :sheet *suit-texture*)))


(defparameter +frame-delay+ (round (/ 1000 60)))


(defun action-key-pressed ()
  (print "Action"))




(defun any-p (arg &rest preds)
  (and preds
    (or (funcall (car preds) arg)
        (apply 'any-p (cons arg (cdr preds))))))

(defun all-p (arg &rest preds)
  (if preds
      (and (funcall (car preds) arg)
           (apply 'all-p (cons arg (cdr preds))))
      t))

(defun set-walk-vec-by-keysdown ()
  (with-slots (walk-vec walk-speed diag-walk-speed) *nance*
    (cond ((< 2 (number-of-dpad-keys-down))
           (setf (car walk-vec) 0)
           (setf (cdr walk-vec) 0))

          ((or (all-p *keys-down* 'keys-down-left 'keys-down-right)
               (all-p *keys-down* 'keys-down-up 'keys-down-down))
           (setf (car walk-vec) 0)
           (setf (cdr walk-vec) 0))

          ((all-p *keys-down* 'keys-down-right 'keys-down-down)
           (setf (car walk-vec) diag-walk-speed)
           (setf (cdr walk-vec) diag-walk-speed))

          ((all-p *keys-down* 'keys-down-right 'keys-down-up)
           (setf (car walk-vec) diag-walk-speed)
           (setf (cdr walk-vec) (* -1 diag-walk-speed)))

          ((all-p *keys-down* 'keys-down-left 'keys-down-down)
           (setf (car walk-vec) (* -1 diag-walk-speed))
           (setf (cdr walk-vec) diag-walk-speed))

          ((all-p *keys-down* 'keys-down-left 'keys-down-up)
           (setf (car walk-vec) (* -1 diag-walk-speed))
           (setf (cdr walk-vec) (* -1 diag-walk-speed)))

          ((keys-down-right *keys-down*)
           (setf (car walk-vec) walk-speed)
           (setf (cdr walk-vec) 0))

          ((keys-down-left *keys-down*)
           (setf (car walk-vec) (* -1 walk-speed))
           (setf (cdr walk-vec) 0))

          ((keys-down-up *keys-down*)
           (setf (car walk-vec) 0)
           (setf (cdr walk-vec) (* -1 walk-speed)))

          ((keys-down-down *keys-down*)
           (setf (car walk-vec) 0)
           (setf (cdr walk-vec) walk-speed))

          (t (setf (car walk-vec) 0)
             (setf (cdr walk-vec) 0)))))


(defun add-walk-hero-left ()
  (setf (face *nance*) 'walking-left)
  (setf (frame *nance*) 0)
  (set-walk-vec-by-keysdown))

(defun add-walk-hero-right ()
  (setf (face *nance*) 'walking-right)
  (setf (frame *nance*) 0)
  (set-walk-vec-by-keysdown))

(defun add-walk-hero-up ()
  (setf (face *nance*) 'walking-up)
  (setf (frame *nance*) 0)
  (set-walk-vec-by-keysdown))

(defun add-walk-hero-down ()
  (setf (face *nance*) 'walking-down)
  (setf (frame *nance*) 0)
  (set-walk-vec-by-keysdown))

(defun rem-walk-hero-left ()
  (set-walk-vec-by-keysdown)
  (unless (walking-p *nance*)
    (setf (face *nance*) 'facing-left)
    (setf (frame *nance*) 0)))

(defun rem-walk-hero-right ()
  (set-walk-vec-by-keysdown)
  (unless (walking-p *nance*)
    (setf (face *nance*) 'facing-right)
    (setf (frame *nance*) 0)))

(defun rem-walk-hero-up ()
  (set-walk-vec-by-keysdown)
  (unless (walking-p *nance*)
    (setf (face *nance*) 'facing-up)
    (setf (frame *nance*) 0)))

(defun rem-walk-hero-down ()
  (set-walk-vec-by-keysdown)
  (unless (walking-p *nance*)
    (setf (face *nance*) 'facing-down)
    (setf (frame *nance*) 0)))

(defstruct keys-down left right up down action)
(defvar *keys-down* (make-keys-down))

(defun number-of-dpad-keys-down ()
   (let ((c 0))
     (dolist (fn '(keys-down-left keys-down-right keys-down-up keys-down-down))
        (when (funcall fn *keys-down*)
          (incf c)))
     c))


(defun handle-keydown (keysym)
  (let ((key (sdl2:scancode-value keysym)))
    (match-key key
      (:scancode-space (unless (keys-down-action *keys-down*)
                         (setf (keys-down-action *keys-down*) t)
                         (action-key-pressed)))

      (:scancode-left (unless (keys-down-left *keys-down*)
                        (setf (keys-down-left *keys-down*) t)
                        (add-walk-hero-left)))

      (:scancode-right (unless (keys-down-right *keys-down*)
                         (setf (keys-down-right *keys-down*) t)
                         (add-walk-hero-right)))

      (:scancode-up (unless (keys-down-up *keys-down*)
                      (setf (keys-down-up *keys-down*) t)
                      (add-walk-hero-up)))

      (:scancode-down (unless (keys-down-down *keys-down*)
                        (setf (keys-down-down *keys-down*) t)
                        (add-walk-hero-down))))))


(defun handle-keyup (keysym)
  (let ((key (sdl2:scancode-value keysym)))
    (match-key key
      (:scancode-left (setf (keys-down-left *keys-down*) nil)
                      (rem-walk-hero-left))

      (:scancode-right (setf (keys-down-right *keys-down*) nil)
                       (rem-walk-hero-right))

      (:scancode-up (setf (keys-down-up *keys-down*) nil)
                    (rem-walk-hero-up))

      (:scancode-down (setf (keys-down-down *keys-down*) nil)
                      (rem-walk-hero-down)))))


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


(defun start-debug ()
  (bt:make-thread (lambda () (swank:create-server :port 4006 :dont-close t)))
  (start))


(defun start ()

  ;; (unless *harmony-initialized-p*
  ;;   (harmony-simple:initialize)
  ;;   (setf *looking-up-track* (harmony-simple:play #p"assets/thingslookup.mp3" :music :loop t))
  ;;   (harmony-simple:stop *looking-up-track*)
  ;;   (setf *cold-day-track* (harmony-simple:play #p"assets/coldday.mp3" :music :loop t))
  ;;   (setf *current-track* *cold-day-track*)
  ;;   (setf *harmony-initialized-p* t))

  ;; (play-track *cold-day-track*)

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
;                 (harmony-simple:stop *current-track*)
                 (free-assets)
                 t))))))

(defun free-assets ()
  (setf (sheet *nance*) nil))

