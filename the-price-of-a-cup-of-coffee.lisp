;;;; the-price-of-a-cup-of-coffee.lisp

(in-package #:the-price-of-a-cup-of-coffee)

(defparameter +window-width+ 1024)
(defparameter +window-height+ 600)
(defparameter +meter-bar-height+ 16)
(defparameter +vert-min+ 32)
(defparameter +vert-max+ (- +window-height+ 128 30))
(defparameter +frame-delay+ (round (/ 1000 60)))

(defvar *nance*)
(defvar *pedestrians* nil)
(defvar *to-render-by-y* nil)
(defvar *on-coffee-break* nil)

(defvar *tweens* nil)

(defvar *expression-rect*
  (sdl2:make-rect 0 0 50 50)
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
  (decoration "stressed")
  shape
  filled-shape
  max-width
  percent)

(defvar *status-meter-decoration-rect* (sdl2:make-rect 0 0 48 48))

(defmethod render ((meter status-meter) renderer)
  (with-slots (color shape filled-shape decoration) meter
    (destructuring-bind (r g b a) color
      (sdl2:set-render-draw-color renderer r g b 100)
      (sdl2:render-fill-rect renderer shape)
      (sdl2:set-render-draw-color renderer r g b a)
      (sdl2:render-fill-rect renderer filled-shape)
      (sdl2:set-render-draw-color renderer r g b 255)
      (sdl2:render-draw-rect renderer shape))
    (setf (sdl2:rect-x *status-meter-decoration-rect*)
          (+ -32 (sdl2:rect-x shape) (sdl2:rect-width shape)))
    (setf (sdl2:rect-y *status-meter-decoration-rect*)
          (+ -16 (sdl2:rect-y shape)))
    (sdl2:render-copy renderer *expression-texture*
                      :source-rect (get-expression decoration)
                      :dest-rect *status-meter-decoration-rect*)))

(defmethod (setf percent) :after (new-val (meter status-meter))
  (with-slots (filled-shape max-width percent) meter
    (setf percent (clamp new-val 0.0 1.0))
    (setf (sdl2:rect-width filled-shape) (round (* max-width percent)))))

(let* ((padding 8)
       (y (- +window-height+ +meter-bar-height+ padding))
       (measure (round (/ +window-width+ 5)))
       (width (- measure (* 2 padding)))
       (double-width (- (* 2 measure) (* 2 padding))))

  (defvar *money-meter*
    (make-instance 'status-meter
                   :color (list 0 200 50 200)
                   :decoration "dollars"
                   :filled-shape (sdl2:make-rect padding y 1 +meter-bar-height+)
                   :shape (sdl2:make-rect padding y double-width +meter-bar-height+)
                   :percent 0.0
                   :max-width double-width))

  (defvar *stress-meter*
    (make-instance 'status-meter
                   :color (list 200 20 20 200)
                   :decoration "stressed"
                   :filled-shape (sdl2:make-rect (+ padding (* 3 measure)) y
                                                 1 +meter-bar-height+)
                   :shape (sdl2:make-rect (+ padding (* 3 measure)) y
                                          width +meter-bar-height+)
                   :percent 0.0
                   :max-width width))

  (defvar *cold-meter*
    (make-instance 'status-meter
                   :color (list 0 140 240 200)
                   :decoration "cold"
                   :filled-shape (sdl2:make-rect (+ padding (* 4 measure)) y 1 +meter-bar-height+)
                   :shape (sdl2:make-rect (+ padding (* 4 measure)) y width +meter-bar-height+)
                   :percent 0.0
                   :max-width width)))

(defmethod render :after ((cold-meter (eql *cold-meter*)) renderer)
  (let ((x (round (* 0.45 (max-width cold-meter)))))
    (setf (sdl2:rect-x *status-meter-decoration-rect*) x)
    (sdl2:render-copy renderer *expression-texture*
                      :source-rect (get-expression "coffee")
                      :dest-rect *status-meter-decoration-rect*)))

(defmethod (setf percent) :after (new-val (meter (eql *stress-meter*)))
  (when (<= 1.0 new-val)
    (stressed-out-sequence)))


;; TODO - REIMPLEMENT TO WORK WITH *TWEENS*
;; (defun drink-coffee ()
;;   (let ((now (sdl2:get-ticks))
;;         (dur 1750)
;;         (ease #'cubic-in-out))
;;     (setf *coffee-break-tween*
;;           (as-group
;;            (animate *cold-meter* 'percent 0.0 :start now :rounding nil :duration dur
;;                     :ease ease)
;;            (animate *stress-meter* 'percent (* 0.4 (percent *stress-meter*))
;;                     :start now :rounding nil :duration dur
;;                     :ease ease)))))

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
           (sdl2:rect-height *expression-rect*))))

(defmethod render ((human human) renderer)
  (with-slots (pos sheet faces face frame expression) human
    (sdl2:render-copy renderer sheet
                      :dest-rect pos
                      :source-rect (get-frame-rect human))
    (let-when (source-rect (and expression (get-expression expression)))
      (set-expression-rect human)
      (sdl2:render-copy renderer *speech-bubble-texture*
                        :dest-rect *expression-rect*)
      (sdl2:render-copy renderer *expression-texture*
                        :dest-rect *expression-rect*
                        :source-rect source-rect))))

(defun emote (who emotion &optional duration)
  (setf (expression who) emotion)
  (when duration
    (let ((pause (pause duration (sdl2:get-ticks))))
      (setf (on-complete pause) (lambda () (setf (expression who) nil)))
      (push pause *tweens*))))

(def-normal-class hero (human)
  (sick-p nil))


(defvar *sickness-rect* (sdl2:make-rect 0 0 16 16))

(defmethod render :after ((nance hero) renderer)
  (when (sick-p nance)
    (setf (sdl2:rect-x *sickness-rect*) (- (x-pos nance) 20))
    (setf (sdl2:rect-y *sickness-rect*) (- (y-pos nance) 20))
    (sdl2:render-copy renderer *expression-texture*
                      :source-rect (get-expression "nauseated")
                      :dest-rect *sickness-rect*)))




(defun make-sick ()
  (unless (sick-p *nance*)
    (setf (sick-p *nance*) t)
    (setf (walk-speed *nance*) (round (* 0.5 (walk-speed *nance*))))))

(defun get-better ()
  (when (sick-p *nance*)
    (setf (sick-p *nance*) nil)
    (setf (walk-speed *nance*) (* 2 (walk-speed *nance*)))))

(defun sickness-check ()
  (if (>= (percent *cold-meter*) 0.9)
      (when (and (not (sick-p *nance*))
                 (cointoss (percent *stress-meter*)))
        (make-sick))
      (when (and (sick-p *nance*)
                 (cointoss (- 1.0 (percent *stress-meter*))))
        (get-better))))

(defun check-sickness-loop ()
  (sickness-check)
  (pause-then 1000 #'check-sickness-loop))

(defvar *collision-on-p* t)
(defvar *input-mode* :normal) ;; (or :normal :start nil)

(defun stressed-out-sequence ()
  (setf *collision-on-p* nil)
  (setf *input-mode* nil)
  (emote *nance* "incapacitated")
  (with-slots (pos face) *nance*
    (let ((move-to-home-base
            (sequencing (:at (sdl2:get-ticks) :targeting pos)
              (grouping (:for 2000)
                (animating :the 'sdl2:rect-x :to +home-base-x+)
                (animating :the 'sdl2:rect-y :to +home-base-y+))
              (take-action (lambda ()
                             (print "calling first callback")
                             (setf face 'facing-down)
                             (emote *nance* "breakdown")))
              (animate *stress-meter* 'percent 0.25
                       :rounding nil :duration 4000 :ease #'quad-in-out
                       :on-complete (lambda ()
                                      (print "calling second callback")
                                      (emote *nance* nil)
                                      (setf *collision-on-p* t)
                                      (setf *input-mode* :normal))))))
      (push move-to-home-base *tweens*))))

(defun in-front-of-door-p ()
  (with-slots (pos) *nance*
    (and (<= +sliding-door-closed-x+
             (sdl2:rect-x pos)
             (+ +sliding-door-closed-x+
                (sdl2:rect-width *sliding-door-position*)))
         (<= (sdl2:rect-y pos) (+ +vert-min+ 40)))))

(defmethod update :after ((hero hero) ticks)
  (with-slots (pos) hero
    (setf (sdl2:rect-x pos)
          (mod (sdl2:rect-x pos) +window-width+))
    (if (in-front-of-door-p)
        (when (not *door-open-p*)
          (open-door))
        (when *door-open-p*
          (close-door)))))

(def-normal-class pedestrian (human)
  (already-asked nil)
  (comfort-rad 60)
  (react-per-sec 4)
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
                         :comfort-rad 120
                         :anger 0.2
                         :kindness 0.5
                         :generosity 0.25
                         :vulnerability 0.07
                         )))
    (setf (walk-speed suit) 5)
    (setf (walk-vec suit) (cons (walk-speed suit) 0))

    (set-walk-face-by-walk-vec suit)
    (setf (sdl2:rect-y (pos suit)) (random-y-pos))
    suit))

(defparameter +home-base-y+ 38)
(defparameter +home-base-x+ 292)

(defun boot-up (renderer)
  ;; cleanup from previous calls to start - used while testing
  (setf *pedestrians* nil)
  (setf *to-render-by-y* nil)

  (boot-up-assets renderer)

  ;; boot up nance
  (setf *nance* (make-instance 'hero :sheet *nance-texture*))
  (setf (sdl2:rect-x (pos *nance*)) 292)
  (setf (sdl2:rect-y (pos *nance*)) 38)

  (push *nance* *to-render-by-y*)

  ;; boot up initial pedestrians
  (push (make-suit) *pedestrians*)
  (push (car *pedestrians*) *to-render-by-y*))


(defun choose-one (&rest options)
  (nth (random (length options)) options))

(defun pause-then (time complete)
  (let ((pause (pause time (sdl2:get-ticks))))
    (setf (on-complete pause) complete)
    (push pause *tweens*)))


(defun stop-and-consider (pedestrian)
  (with-slots (walk-vec already-asked expression anger kindness generosity vulnerability) pedestrian
    (incf (percent *stress-meter*) vulnerability)
    (setf already-asked t)
    (when (walking-p pedestrian)
      (let ((old-vec (copy-list walk-vec)))
        (setf (car walk-vec) 0)
        (setf (cdr walk-vec) 0)
        (emote pedestrian "alarmed-question" 800)
        (emote *nance* "alarmed-question" 800)
        (pause-then 1000 (lambda ()
                           (cond
                             ((cointoss anger)
                              (emote pedestrian (choose-one "asshole" "very-angry" "death") 2500)
                              (hopping-mad pedestrian)
                              (emote *nance* (choose-one "stressed" "breakdown") 3000)
                              (incf (percent *stress-meter*) (* 3 vulnerability)))
                             ((cointoss kindness)
                              (emote pedestrian (choose-one "relaxed" "heart1" "heart2" "heart3" "angry") 2500)
                              (emote *nance* (choose-one "relaxed" "heart1" "heart2" "heart3") 2000)
                              (incf (percent *money-meter*) (random generosity)))
                             (t (emote pedestrian (choose-one "sorry-no" "neutral") 2500)))
                           (resume-walking pedestrian old-vec 800)))))))

(defun resume-walking (person vec after)
  (let ((pause (pause after (sdl2:get-ticks))))
    (setf (on-complete pause)
          (lambda ()
            (setf (car (walk-vec person))
                  (car vec))
            (setf (cdr (walk-vec person))
                  (cdr vec))))
    (push pause *tweens*)))

(defun action-key-pressed ()
  (let-if (mark (find-if (lambda (ped)
                           (and (not (already-asked ped))
                                (< (dist ped *nance*)
                                   (* 0.75 (comfort-rad ped)))))
                         *pedestrians*))
          (stop-and-consider mark)))




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
      (:scancode-space (setf (keys-down-action *keys-down*) nil))
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

(defun cointoss (&optional (loaded 0.5))
  (< (random 1.0) loaded))

(defun adjust-walk-relative-to (person1 person2)
  (when (walking-p person1)
    (with-slots (comfort-rad walk-vec walk-speed diag-walk-speed vulnerability) person1
      (if (< (dist person1 person2) comfort-rad)
          ;;move-away
          (match walk-vec
            ((cons old-dx 0)
             (when (cointoss (* 5 vulnerability)) (emote person1 "alarmed" 1000))
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
      (setf (already-asked ped) nil)
      (setf (sdl2:rect-y pos) (random-y-pos))
      (setf (sdl2:rect-x pos) -49))))


(defmethod update ((game (eql :game)) time)
  (update *nance* time)

  (update-tweens time)

  (dolist (person *pedestrians*)
    (update person time))

  (unless *on-coffee-break*
    (if (walking-p *nance*)
        (decf (percent *cold-meter*) 0.0004)
        (incf (percent *cold-meter*) 0.0003))))




(defmethod render ((game (eql :game)) renderer)
  ;; clear screen
  (sdl2:set-render-draw-color renderer 80 80 80 255)
  (sdl2:render-clear renderer)

  (sdl2:render-copy renderer *backdrop-texture*)
  (sdl2:render-copy renderer *sliding-door-texture*
                    :dest-rect *sliding-door-position*)

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


(defvar *door-open-p* nil)

(defun open-door ()
  (unless *door-open-p*
    (setf *door-open-p* t)
    (push (animate *sliding-door-position* 'sdl2:rect-x +sliding-door-open-x+
                   :start (sdl2:get-ticks)
                   :ease #'animise:cubic-in-out
                   :duration 500)
          *tweens*)))

(defun close-door ()
  (when *door-open-p*
    (setf *door-open-p* nil)
    (push (animate *sliding-door-position* 'sdl2:rect-x +sliding-door-closed-x+
                   :start (sdl2:get-ticks)
                   :ease #'animise:cubic-in-out
                   :duration 500)
          *tweens*)))

;; TODO FIX STRANGE BUG IN FINITE LOOPING BEHAVIOR IN ANIMISE
(defun hopping-mad (who)
  (with-slots (pos) who
    (let* ((current-y (sdl2:rect-y pos))
           (dest-y (- current-y 56))
           (anim
             (sequencing (:at (sdl2:get-ticks) :targeting pos)
               (animating :the 'sdl2:rect-y :to dest-y :for 200 :by :quading-out)
               (animating :the 'sdl2:rect-y :to current-y :for 200 :by :elastic-out)
               (animating :the 'sdl2:rect-y :to dest-y :for 200 :by :quading-out)
               (animating :the 'sdl2:rect-y :to current-y :for 200 :by :elastic-out)
               (animating :the 'sdl2:rect-y :to dest-y :for 200 :by :quading-out)
               (animating :the 'sdl2:rect-y :to current-y :for 200 :by :elastic-out)
               )))
      (push anim *tweens*))))
