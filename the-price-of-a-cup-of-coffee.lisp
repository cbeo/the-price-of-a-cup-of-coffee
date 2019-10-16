;;;; the-price-of-a-cup-of-coffee.lisp

(in-package #:the-price-of-a-cup-of-coffee)

(defparameter +window-width+ 1024)
(defparameter +window-height+ 600)
(defparameter +meter-bar-height+ 16)
(defparameter +vert-min+ 16)
(defparameter +vert-max+ (- +window-height+ 128 10))
(defparameter +frame-delay+ (round (/ 1000 60)))

(defvar *nance*)
(defvar *pedestrians* nil)
(defvar *to-render-by-y* nil)

(defvar *tweens* nil)

(defvar *expression-rect*
  (sdl2:make-rect 0 0 32 32)
  "used to render expressions.")


(defstruct keys-down left right up down action)
(defvar *keys-down* (make-keys-down))

(defvar *human-frame-pause* (/ 1000 4))
(defun set-human-fps (n)
  (setf *human-frame-pause* (/ 1000 n)))

(defgeneric render (sprite renderer))
(defgeneric update (thing time))

(def-normal-class status-meter ()
  (color (list 0 0 0 255))
  shape
  filled-shape
  max-width
  percent)

(defmethod render ((meter status-meter) renderer)
  (with-slots (color shape filled-shape) meter
    (destructuring-bind (r g b a) color
      (sdl2:set-render-draw-color renderer r g b 100)
      (sdl2:render-fill-rect renderer shape)
      (sdl2:set-render-draw-color renderer r g b a)
      (sdl2:render-fill-rect renderer filled-shape)
      (sdl2:set-render-draw-color renderer r g b 255)
      (sdl2:render-draw-rect renderer shape))))

(defmethod (setf percent) :after (new-val (meter status-meter))
  (with-slots (filled-shape max-width percent) meter
    (setf percent (clamp new-val 0.0 1.0))
    (setf (sdl2:rect-width filled-shape) (round (* max-width percent)))))

(let* ((padding 8)
       (measure (round (/ +window-width+ 5)))
       (width (- measure (* 2 padding)))
       (double-width (- (* 2 measure) (* 2 padding))))

  (defvar *money-meter*
    (make-instance 'status-meter
                   :color (list 0 200 50 200)
                   :filled-shape (sdl2:make-rect padding padding 1 +meter-bar-height+)
                   :shape (sdl2:make-rect padding padding double-width +meter-bar-height+)
                   :percent 0.0
                   :max-width double-width))

  (defvar *stress-meter*
    (make-instance 'status-meter
                   :color (list 200 20 20 200)
                   :filled-shape (sdl2:make-rect (+ padding (* 3 measure)) padding
                                                 1 +meter-bar-height+)

                   :shape (sdl2:make-rect (+ padding (* 3 measure)) padding
                                          width +meter-bar-height+)
                   :percent 0.0
                   :max-width width))

  (defvar *cold-meter*
    (make-instance 'status-meter
                   :color (list 0 40 204 200)
                   :filled-shape (sdl2:make-rect (+ padding (* 4 measure)) padding 1 +meter-bar-height+)
                   :shape (sdl2:make-rect (+ padding (* 4 measure)) padding width +meter-bar-height+)
                   :percent 0.0
                   :max-width width)))


(defvar *coffee-break-tween* nil)
(defun drink-coffee ()
  (let ((now (sdl2:get-ticks))
        (dur 1750)
        (ease #'cubic-in-out))
    (setf *coffee-break-tween*
          (as-group
           (animate *cold-meter* 'percent 0.0 :start now :rounding nil :duration dur
                    :ease ease)
           (animate *stress-meter* 'percent (* 0.4 (percent *stress-meter*))
                    :start now :rounding nil :duration dur
                    :ease ease)))))

(defun get-sick ()
  (unless (sick-p *nance*)
    (setf (sick-p *nance*) t)
    (setf (walk-speed *nance*)
          (round (* 0.5 (walk-speed *nance*))))))

(defun get-better ()
  (when (sick-p *nance*)
    (setf (sick-p *nance*) nil)
    (setf (walk-speed *nance*)
          (round (* 2 (walk-speed *nance*))))))


(def-normal-class human ()
  (walk-vec (cons 0 0))
  (walk-speed 6)
  diag-walk-speed
  expression       ;; nil or a string
  pos
  sheet
  (faces ((lambda () +shared-faces+)))
  (face 'facing-down)
  (frame 0)
  (next-frame-at 0))

(defun x-pos (person)
  (sdl2:rect-x (pos person)))

(defun y-pos (person)
  (sdl2:rect-y (pos person)))

(defun walking-p (human)
  (not (standing-p human)))

(defun standing-p (human)
  (and (zerop (car (walk-vec human)))
       (zerop (cdr (walk-vec human)))))


(defmethod (setf walk-speed) :after (newval (human human))
  (setf (diag-walk-speed human)
        (round (sqrt (* 0.5 (* newval newval))))))

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



(defmethod update ((human human) ticks)
  (with-slots (frame next-frame-at faces face walk-vec pos) human
    (incf (sdl2:rect-x pos) (car walk-vec))
    (setf (sdl2:rect-y pos)
          (clamp (+ (sdl2:rect-y pos) (cdr walk-vec))
                 +vert-min+ +vert-max+))
    (when (<= next-frame-at ticks)
      (setf next-frame-at (max (+ *human-frame-pause* next-frame-at) ticks))
      (setf frame (mod (1+ frame) (length (funcall face faces)))))))


(defun set-expression-rect (human)
  (setf (sdl2:rect-x *expression-rect*)
        (sdl2:rect-x (pos human)))
  (setf (sdl2:rect-y *expression-rect*)
        (- (sdl2:rect-y (pos human))
           36)))

(defmethod render ((human human) renderer)
  (with-slots (pos sheet faces face frame expression) human
    (sdl2:render-copy renderer sheet
                      :dest-rect pos
                      :source-rect (get-frame-rect human))
    (when expression
      (set-expression-rect human)
      (sdl2:render-copy renderer *expression-texture*
                        :dest-rect *expression-rect*
                        :source-rect (get-expression expression)))))

(defun emote (who emotion &optional duration)
  (setf (expression who) emotion)
  (when duration
    (let ((pause (pause duration (sdl2:get-ticks))))
      (setf (on-complete pause) (lambda () (setf (expression who) nil)))
      (push pause *tweens*))))

(def-normal-class hero (human)
  (sick-p nil))

(defun make-sick (hero)
  (unless (sick-p hero)
    (setf (sick-p hero) t)
    (setf (walk-speed hero) (round (* 0.5 (walk-speed hero))))))

(defun get-better (hero)
  (when (sick-p hero)
    (setf (sick-p hero) nil)
    (setf (walk-speed hero) (* 2 (walk-speed hero)))))


(defmethod update :after ((hero hero) ticks)
  (with-slots (pos) hero
    (setf (sdl2:rect-x pos)
          (mod (sdl2:rect-x pos) +window-width+))))


(def-normal-class pedestrian (human)
  (comfort-rad 60)
  (react-per-sec 3)
  (next-react 0)
  (anger 0.1)
  (kindness 0.02)
  (generosity 0.25)
  (vulnerability 0.03))

(defun set-walk-face-by-walk-vec (person)
  (with-slots (walk-vec face) person
    (cond
      ((and (zerop (cdr walk-vec)) (plusp (car walk-vec)))
       (setf face 'walking-right))

      ((and (zerop (cdr walk-vec)) (minusp (car walk-vec)))
       (setf face 'walking-left))

      ((minusp (cdr walk-vec))  (setf face 'walking-up))

      ((plusp (cdr walk-vec))  (setf face 'walking-down))

      (t nil)))) ;; return nil if the character is standing

(defun random-y-pos ()
  (+ +vert-min+ (random (- +vert-max+ +vert-min+))))

(defun make-suit ()
  (let ((suit
          (make-instance 'pedestrian
                         :sheet *suit-texture*
                         :comfort-rad 60
                         :anger 0.1
                         :kindness 0.015
                         :generosity 0.5
                         :vulnerability 0.01
                         )))
    (setf (walk-speed suit) 4)
    (setf (walk-vec suit) (cons (walk-speed suit) 0))

    (set-walk-face-by-walk-vec suit)
    (setf (sdl2:rect-y (pos suit)) (random-y-pos))
    suit))




(defun boot-up (renderer)
  (boot-up-assets renderer)

  ;; boot up nance
  (setf *nance* (make-instance 'hero :sheet *nance-texture*))
  (setf (sdl2:rect-x (pos *nance*))
        (round (* 0.5 (- +window-width+ (sdl2:rect-width (pos *nance*))))))

  (push *nance* *to-render-by-y*)

  ;; boot up initial pedestrians
  (setf *pedestrians* nil)
  (push (make-suit) *pedestrians*)
  (push (car *pedestrians*) *to-render-by-y*))



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

(defun update-tweens (time)
  (dolist (tween *tweens*)
    (run-tween tween time))
  (setf *tweens*
        (delete-if ($ #'tween-finished-p _ time)
                   *tweens*)))


(defun dist (person1 person2)
  (let ((dx (- (x-pos person1) (x-pos person2)))
        (dy (- (y-pos person1) (y-pos person2))))
    (sqrt (+ (* dx dx) (* dy dy)))))

(defun adjust-walk-relative-to (person1 person2)
  (when (walking-p person1)
    (with-slots (comfort-rad walk-vec walk-speed diag-walk-speed) person1
      (if (< (dist person1 person2) comfort-rad)
          ;;move-away
          (match walk-vec
            ((cons old-dx 0)
             (setf (car walk-vec) (* (signum old-dx) diag-walk-speed))
             (setf (cdr walk-vec) (* (signum (- (y-pos person1) (y-pos person2)))
                                     diag-walk-speed))))
          ;; try moving straigt across again
          (match walk-vec
            ((cons old-dx _)
             (setf (car walk-vec) (* (signum old-dx) walk-speed))
             (setf (cdr walk-vec) 0)))))
    ;; update the displayd animation
    (set-walk-face-by-walk-vec person1)))

(defmethod update ((ped pedestrian) time)
  (call-next-method)
  (with-slots (pos react-per-sec next-react) ped
    (when (<= next-react time)
      ;; update react check
      (setf next-react
            (max time
                 (round (+ next-react
                           (/ 1000 react-per-sec)))))
      (adjust-walk-relative-to ped *nance*))

    (when (or (< (sdl2:rect-x pos) -50)
              (< +window-width+ (sdl2:rect-x pos)))
      (setf (sdl2:rect-y pos) (random-y-pos))
      (setf (sdl2:rect-x pos) -49))))


(defmethod update ((game (eql :game)) time)
  (update *nance* time)

  (update-tweens time)

  (dolist (person *pedestrians*)
    (update person time))

  (unless *coffee-break-tween*
    (if (walking-p *nance*)
        (decf (percent *cold-meter*) 0.0004)
        (incf (percent *cold-meter*) 0.0005))))



(defmethod render ((game (eql :game)) renderer)
  ;; clear screen
  (sdl2:set-render-draw-color renderer 80 80 80 255)
  (sdl2:render-clear renderer)

  ;; render characters and other objects
  (setf *to-render-by-y*
        (sort *to-render-by-y* #'< :key #'y-pos))
  (dolist (person *to-render-by-y*)
    (render person renderer))

  ;; render meters
  (sdl2:set-render-draw-blend-mode renderer sdl2-ffi:+sdl-blendmode-blend+)
  (render *money-meter* renderer)
  (render *stress-meter* renderer)
  (render *cold-meter* renderer)

  ;; present
  (sdl2:render-present renderer))

(defun play-track (track)
  (harmony-simple:stop *current-track*)
  (harmony-simple:resume track)
  (setf *current-track* track))


(defun start-debug ()
  (bt:make-thread (lambda () (swank:create-server :port 4006 :dont-close t)))
  (start))


(defun start ()

  ;; (play-track *cold-day-track*)
  (unwind-protect 
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
                      (update :game (sdl2:get-ticks))
                      (render :game renderer)
                      (sdl2:delay +frame-delay+))
               (:quit () t)))))
    (free-assets)))


(defun clear-pedestrians ()
  (dolist (p *pedestrians*)
    (setf *to-render-by-y*
          (delete p *to-render-by-y*)))
  (setf *pedestrians* nil))

