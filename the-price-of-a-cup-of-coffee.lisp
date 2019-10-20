;;;; the-price-of-a-cup-of-coffee.lisp

(in-package #:the-price-of-a-cup-of-coffee)

;;; CONSTANTS
(defparameter +window-width+ 1024)
(defparameter +window-height+ 600)
(defparameter +meter-bar-height+ 16)
(defparameter +vert-min+ 32)
(defparameter +vert-max+ (- +window-height+ 128 30))
(defparameter +frame-delay+ (round (/ 1000 60)))
(defparameter +home-base-y+ 36)
(defparameter +home-base-x+ 292)
(defparameter +coffee-cost+ 0.45)
(defparameter +screen-sized-rect+ nil)


;;; STUCTS AND CLASSES
(defstruct keys-down left right up down action)

(def-normal-class status-meter ()
  (color (list 0 0 0 255))
  (decoration "stressed")
  shape
  filled-shape
  max-width
  percent)

(def-normal-class human ()
  (paused nil)
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

(def-normal-class hero (human)
  (sick-p nil))

(def-normal-class pedestrian (human)
  (already-asked nil)
  (comfort-rad 60)
  (react-per-sec 4)
  (next-react 0)
  (anger 0.1)
  (kindness 0.02)
  (generosity 0.25)
  (vulnerability 0.03))

;;; GLOBALS
(defvar *nance*)
(defvar *special-nance* t)
(defvar *pedestrians* nil)
(defvar *to-render-by-y* nil)
(defvar *on-coffee-break* nil)
(defvar *status-meter-decoration-rect*)
(defvar *space-clamping-p* t)
(defvar *tweens* nil)
(defvar *sickness-rect*)
(defvar *collision-on-p* t)
(defvar *input-mode* :start) ;; (or :normal :start nil)
(defvar *collision-count* 0)
(defvar *ped-hit-box* )
(defvar *nance-hit-box* )
(defvar *fading-out* nil)
(defvar *door-open-p* nil)
(defvar *expression-rect*)

(defvar *keys-down*)
(defvar *human-frame-pause* (/ 1000 4))

(defvar *cached-pedestrians* nil)
(defvar *pedestrian-count* 4)
(defvar *money-meter* nil)
(defvar *stress-meter* nil)
(defvar *cold-meter* nil)


(defun boot-meters ()

  (let* ((padding 8)
         (y (- +window-height+ +meter-bar-height+ padding))
         (measure (round (/ +window-width+ 5)))
         (width (- measure (* 2 padding)))
         (double-width (- (* 2 measure) (* 2 padding))))

  (setf *money-meter*
    (make-instance 'status-meter
                   :color (list 0 200 50 200)
                   :decoration "dollars"
                   :filled-shape (sdl2:make-rect padding y 1 +meter-bar-height+)
                   :shape (sdl2:make-rect padding y double-width +meter-bar-height+)
                   :percent 0.0
                   :max-width double-width))

  (setf *stress-meter*
    (make-instance 'status-meter
                   :color (list 200 20 20 200)
                   :decoration "stressed"
                   :filled-shape (sdl2:make-rect (+ padding (* 3 measure)) y
                                                 1 +meter-bar-height+)
                   :shape (sdl2:make-rect (+ padding (* 3 measure)) y
                                          width +meter-bar-height+)
                   :percent 0.0
                   :max-width width))

  (setf *cold-meter*
    (make-instance 'status-meter
                   :color (list 0 140 240 200)
                   :decoration "cold"
                   :filled-shape (sdl2:make-rect (+ padding (* 4 measure)) y 1 +meter-bar-height+)
                   :shape (sdl2:make-rect (+ padding (* 4 measure)) y width +meter-bar-height+)
                   :percent 0.0
                   :max-width width)))

  (defmethod render :after ((money-meter (eql *money-meter*)) renderer)
    (let ((x (round (* +coffee-cost+ (max-width money-meter)))))
      (setf (sdl2:rect-x *status-meter-decoration-rect*) x)
      (sdl2:render-copy renderer *expression-texture*
                        :source-rect (get-expression "coffee")
                        :dest-rect *status-meter-decoration-rect*)))

  (defmethod (setf percent) :after (new-val (meter (eql *stress-meter*)))
    (when (<= 1.0 new-val)
      (stressed-out-sequence)))

  )

;;; GENERICS
(defgeneric render (sprite renderer))
(defgeneric update (thing time))


;;;; INITIALIZERS
(defmethod initialize-instance :after ((human human) &key)
  (setf (walk-speed human) 6)
  (with-slots (faces pos) human
    (let ((rect (get-frame-rect human)))
      (setf pos
            (sdl2:make-rect 0 0
                            (sdl2:rect-width rect)
                            (sdl2:rect-height rect))))))



;;; RENDERING IMPLEMENTATIONS
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

(defmethod render :after ((nance hero) renderer)
  (when (sick-p nance)
    (setf (sdl2:rect-x *sickness-rect*) (- (x-pos nance) 20))
    (setf (sdl2:rect-y *sickness-rect*) (- (y-pos nance) 20))
    (sdl2:render-copy renderer *expression-texture*
                      :source-rect (get-expression "nauseated")
                      :dest-rect *sickness-rect*)))

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

  (when *fading-out*
    (apply #'sdl2:set-render-draw-color (list* renderer *fading-out*))
    ;(sdl2:set-render-draw-color renderer #x94 #x94 #x94 (car *fading-out*))
    (sdl2:render-fill-rect renderer +screen-sized-rect+)
    (when *special-nance*
      (render *nance* renderer)))

  ;; present
  (sdl2:render-present renderer))


;;; UPDATE IMPLEMENTATIONS
(defmethod update ((human human) ticks)
  (with-slots (frame next-frame-at faces face walk-vec pos) human
    (incf (sdl2:rect-x pos) (car walk-vec))
    (setf (sdl2:rect-y pos)
          (if *space-clamping-p*
              (clamp (+ (sdl2:rect-y pos) (cdr walk-vec))
                     +vert-min+ +vert-max+)
              (+ (sdl2:rect-y pos) (cdr walk-vec))))
    (when (<= next-frame-at ticks)
      (setf next-frame-at (max (+ *human-frame-pause* next-frame-at) ticks))
      (setf frame (mod (1+ frame) (length (funcall face faces)))))))

(defmethod update :after ((hero hero) ticks)
  (with-slots (pos) hero
    (setf (sdl2:rect-x pos)
          (mod (sdl2:rect-x pos) +window-width+))
    (snap-hit-box-to hero *nance-hit-box*)
    (if (in-front-of-door-p)
        (when (not *door-open-p*)
          (open-door))
        (when *door-open-p*
          (close-door)))))

(defmethod update ((ped pedestrian) time)
  (unless (paused ped)
    (call-next-method))
  (with-slots (pos react-per-sec next-react) ped
    (when (<= next-react time)
      ;; update react check
      (setf next-react
            (max time
                 (round (+ next-react
                           (/ 1000 react-per-sec)))))
      (adjust-walk-relative-to ped *nance*))

    (collision-check ped)

    (when (or (< (sdl2:rect-x pos) -65)
              (< (+ 65 +window-width+) (sdl2:rect-x pos)))
      (remove-pedestrian ped))))


(defmethod update ((game (eql :game)) time)
  (update *nance* time)
  (update-tweens time)
  (dolist (person *pedestrians*)
    (update person time))
  (unless *on-coffee-break*
    (if (walking-p *nance*)
        (unless (sick-p *nance*)
          (decf (percent *cold-meter*) 0.0002))
        (progn
          (incf (percent *cold-meter*) 0.0005)
          (unless (sick-p *nance*)
            (decf (percent *stress-meter*) 0.0001))))))


;;; ACCESSORS
(defmethod (setf percent) :after (new-val (meter status-meter))
  (with-slots (filled-shape max-width percent) meter
    (setf percent (clamp new-val 0.0 1.0))
    (setf (sdl2:rect-width filled-shape) (round (* max-width percent)))))



(defmethod (setf walk-speed) :after (newval (human human))
  (setf (diag-walk-speed human)
        (round (sqrt (* 0.5 (* newval newval))))))

(defun x-pos (person)
  (sdl2:rect-x (pos person)))

(defun (setf x-pos) (new-val person)
  (setf (sdl2:rect-x (pos person)) new-val))


(defun y-pos (person)
  (sdl2:rect-y (pos person)))

(defun (setf y-pos) (new-val person)
  (setf (sdl2:rect-y (pos person)) new-val))

;;;; PREDICATES
(defun walking-p (human)
  (not (standing-p human)))

(defun standing-p (human)
  (and (zerop (car (walk-vec human)))
       (zerop (cdr (walk-vec human)))))

(defun in-front-of-door-p ()
  (with-slots (pos) *nance*
    (and (<= +sliding-door-closed-x+
             (sdl2:rect-x pos)
             (+ +sliding-door-closed-x+
                (sdl2:rect-width *sliding-door-position*)))
         (<= (sdl2:rect-y pos) (+ +vert-min+ 40)))))

(defun enough-for-coffee-p ()
  (<= +coffee-cost+ (percent *money-meter*)))

(defun enough-for-food-p ()
  (<= 1.0 (percent *money-meter*)))


;;;; STATE CONTROL
(defun make-sick ()
  (unless (sick-p *nance*)
    (setf (sick-p *nance*) t)
    (setf (walk-speed *nance*) (round (* 0.5 (walk-speed *nance*))))))

(defun get-better ()
  (when (sick-p *nance*)
    (setf (sick-p *nance*) nil)
    (setf (walk-speed *nance*) (* 2 (walk-speed *nance*)))))

(defun sickness-check ()
  (if (cointoss (percent *cold-meter*))
      (when (and (not (sick-p *nance*))
                 (cointoss (percent *stress-meter*)))
        (make-sick))
      (when (and (sick-p *nance*)
                 (cointoss (- 1.0 (percent *stress-meter*))))
        (get-better))))

(defun check-sickness-loop ()
  (sickness-check)
  (pause-then 3000 #'check-sickness-loop))

(defun boot-up (renderer)
  ;; cleanup from previous calls to start - used while testing
  (setf *pedestrians* nil)
  (setf *to-render-by-y* nil)

  (boot-up-assets renderer)

  ;; boot up nance
  (setf *nance* (make-instance 'hero :sheet *nance-texture*))
  (setf (sdl2:rect-x (pos *nance*)) +home-base-x+)
  (setf (sdl2:rect-y (pos *nance*)) +home-base-y+)

  (push *nance* *to-render-by-y*)

  (check-sickness-loop)
  (spawn-pedestrian-loop))


;;; SPAWNING PEDESTRIANS

;;;; CONSTRUCTORS
(defun make-suit ()
  (let ((suit
          (make-instance 'pedestrian
                         :sheet *suit-texture*
                         :comfort-rad 120
                         :anger 0.1
                         :kindness 0.18
                         :generosity 0.25
                         :vulnerability 0.04
                         )))
    (setf (walk-speed suit) 5)
    (setf (walk-vec suit) (cons (walk-speed suit) 0))

    (set-walk-face-by-walk-vec suit)
    (setf (sdl2:rect-y (pos suit)) (random-y-pos))
    suit))

(defun make-normy ()
  (let ((normy
          (make-instance 'pedestrian
                         :sheet *normy-texture*
                         :comfort-rad 90
                         :anger 0.07
                         :kindness 0.24
                         :generosity 0.2
                         :vulnerability 0.05)))
    (setf (walk-speed normy) 4)
    (setf (walk-vec normy) (cons (walk-speed normy) 0))
    (setf (sdl2:rect-y (pos normy)) (random-y-pos))
    normy))

(defun make-kid ()
  (let ((kid
          (make-instance 'pedestrian
                         :sheet *kid-texture*
                         :comfort-rad 80
                         :anger 0.04
                         :kindness 0.8
                         :generosity 0.08
                         :vulnerability 0.09)))
    (setf (walk-speed kid) 3)
    (setf (walk-vec kid) (cons (walk-speed kid) 0))
    (setf (sdl2:rect-y (pos kid)) (random-y-pos))
    kid))

(defun make-punker ()
  (let ((punker
          (make-instance 'pedestrian
                         :sheet *punker-texture*
                         :comfort-rad 100
                         :anger 0.01
                         :kindness 0.31
                         :generosity 0.5
                         :vulnerability 0.01)))
    (setf (walk-speed punker) 3)
    (setf (walk-vec punker) (cons (walk-speed punker) 0))
    (setf (sdl2:rect-y (pos punker)) (random-y-pos))
    punker))

(defun add-pedestrian (ped)
  (reset-pedestrian ped)
  (push ped *pedestrians*)
  (push ped *to-render-by-y*))

(defun remove-pedestrian (ped)
  (setf *to-render-by-y* (delete ped *to-render-by-y*))
  (setf *pedestrians* (delete ped *pedestrians*)))

(defun spawn-pedestrian ()
  (if *cached-pedestrians*
      (add-pedestrian (pop *cached-pedestrians*))
      (let ((roll (random 100)))
        (cond ((< roll 50) (add-pedestrian (make-normy)))
              ((< roll 75) (add-pedestrian (make-suit)))
              ((< roll 90) (add-pedestrian (make-kid)))
              (t (add-pedestrian (make-punker)))))))


(defun spawn-pedestrian-loop ()
  (when (< (length *pedestrians*) *pedestrian-count*)
    (spawn-pedestrian))
  (pause-then 1200 #'spawn-pedestrian-loop))

;;;; ANIMATIONS AND SEQUENCES
(defun update-tweens (time)
  (dolist (tween *tweens*)
    (run-tween tween time))
  (setf *tweens*
        (delete-if ($ #'tween-finished-p _ time)
                   *tweens*)))

(defun fade-into-game ()
  (setf *fading-out* (list #x94 #x94 #x94 255))
  (setf *input-mode* nil)
  (push (animate *fading-out* 'cadddr 0
                 :start (sdl2:get-ticks)
                 :duration 4000
                 :on-complete
                 (lambda ()
                   (setf *special-nance* nil)
                   (setf *fading-out* nil)
                   (setf *input-mode* :normal)
                   (play-track *cold-day-track*)))
        *tweens*))


(defun emote (who emotion &optional duration)
  (setf (expression who) emotion)
  (when duration
    (let ((pause (pause duration (sdl2:get-ticks))))
      (setf (on-complete pause) (lambda () (setf (expression who) nil)))
      (push pause *tweens*))))

(defun pause-then (time complete)
  (let ((pause (pause time (sdl2:get-ticks))))
    (setf (on-complete pause) complete)
    (push pause *tweens*)))

(defun stressed-out-sequence ()
  (setf *collision-on-p* nil)
  (setf *input-mode* nil)
  (setf (walk-vec *nance*) (cons 0 0))
  (clear-keys-down)
  (emote *nance* "breakdown")
  (with-slots (pos face) *nance*
    (let ((move-to-home-base
            (sequencing (:at (sdl2:get-ticks) :targeting pos)
              (grouping (:for 2000)
                (animating :the 'sdl2:rect-x :to +home-base-x+)
                (animating :the 'sdl2:rect-y :to +home-base-y+))
              (take-action (lambda ()
                             (setf face 'facing-down)
                             (emote *nance* "incapacitated")))
              (animate *stress-meter* 'percent 0.25
                       :rounding nil :duration 4000 :ease #'quad-in-out
                       :on-complete (lambda ()
                                      (emote *nance* nil)
                                      (setf *collision-on-p* t)
                                      (setf *input-mode* :normal))))))
      (push move-to-home-base *tweens*))))

(defun stop-and-consider (pedestrian)
  (setf *collision-on-p* nil)
  (with-slots (walk-vec already-asked expression anger kindness generosity vulnerability) pedestrian
    (setf already-asked t)
    (when (walking-p pedestrian)
      (let ((old-vec (copy-list walk-vec)))
        (setf (car walk-vec) 0)
        (setf (cdr walk-vec) 0)
        (emote pedestrian "alarmed-question" 800)
        (emote *nance* "alarmed-question" 800)
        (pause-then 1000 (lambda ()
                           (setf *collision-on-p* t)
                           (cond
                             ((cointoss anger)
                              (emote pedestrian (choose-one "asshole" "very-angry" "death") 2500)
                              (hopping-mad pedestrian)
                              (emote *nance* (choose-one "stressed" "breakdown") 3000)
                              (incf (percent *stress-meter*) (* 4 vulnerability)))
                             ((cointoss kindness)
                              (emote pedestrian (choose-one "relaxed" "heart1" "heart2" "heart3" "angry") 2500)
                              (emote *nance* (choose-one "relaxed" "heart1" "heart2" "heart3") 2000)
                              (incf (percent *stress-meter*) vulnerability)
                              (incf (percent *money-meter*) (+ 0.05 (random generosity))))
                             (t
                              (incf (percent *stress-meter*) (* 1.7 vulnerability))
                              (emote pedestrian (choose-one "sorry-no" "neutral") 2500)))
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


(defun get-coffee! ()
  (decf (percent *money-meter*) +coffee-cost+)
  (play-track *looking-up-track*)
  (emote *nance* "coffee")
  (setf *collision-on-p* nil)
  (setf *on-coffee-break* t)
  (let ((now (sdl2:get-ticks))
        (dur 10000))
    (push (animate *cold-meter* 'percent 0.25  :duration dur :start now :rounding nil) *tweens*)
    (push (animate *stress-meter* 'percent 0.25  :duration dur :start now :rounding nil) *tweens*)
    (pause-then
     dur
     (lambda ()
       (play-track *cold-day-track*)
       (emote *nance* nil)
       (setf *collision-on-p* t)
       (setf *on-coffee-break* nil)))))

(defun collision-check (ped)
  (when *collision-on-p*
    (snap-hit-box-to ped *ped-hit-box*)
    (when (sdl2:has-intersect *ped-hit-box* *nance-hit-box*)
      (run-collision ped))))


(defun run-collision (ped)
  (setf *collision-on-p* nil)
  (setf *input-mode* nil)
  (clear-keys-down)
  (emote ped (choose-one "very-angry" "angry" "alarmed" "asshole" "death") 2000)
  (emote *nance* (choose-one "angry" "alarmed" "incapacitated" "stressed") 2000)
  (hopping-mad ped)
  (with-slots (pos walk-vec) *nance*
    (let ((now (sdl2:get-ticks))
          (tx (+ (sdl2:rect-x pos) (* (x-direction ped) 80)))
          (oy (sdl2:rect-y pos))
          (ty (+ (sdl2:rect-y pos) -52)))
      (setf (car walk-vec) 0)
      (setf (cdr walk-vec) 0)

      (push (animating :the 'sdl2:rect-x :of pos :to tx :at now :for 250) *tweens*)
      (push (sequencing (:at now :targeting pos)
             (animating :the 'sdl2:rect-y :to ty :by :quad-in :for 225)
             (animating :the 'sdl2:rect-y :to oy :by :quad-out :for 225))
            *tweens*)
      (incf *collision-count*)
      ;; if you've had 3 or more collisions, its a cointoss that somebody calls the cops
      (if (and (>= *collision-count* 3) (cointoss 0.5))
          (pause-then 1000 #'game-over)
          (pause-then 1200
                      (lambda ()
                        (setf *collision-on-p* t)
                        (setf *input-mode* :normal)
                        (incf (percent *stress-meter*) (* 5 (vulnerability ped)))))))))

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



(defun get-food! ()
  (setf (percent *money-meter*) 0.0)
  (play-track *looking-up-track*)
  (emote *nance* (choose-one "food1" "food2" "food3" "food4" "food5"))
  (setf *collision-on-p* 0)
  (setf *collision-count* 0)
  (setf *on-coffee-break* t)
  (fade-out)
  (let ((now (sdl2:get-ticks))
        (dur 5000))
    (push (animate *cold-meter* 'percent 0.0  :duration dur :start now :rounding nil) *tweens*)
    (push (animate *stress-meter* 'percent 0.0  :duration dur :start now :rounding nil) *tweens*)
    (pause-then dur
                (lambda ()
                  (fade-in)
                  (setf *pedestrian-count* (* 2 *pedestrian-count*))
                  (setf (x-pos *nance*) +home-base-x+)
                  (setf (y-pos *nance*) +home-base-y+)
                  (play-track *cold-day-track*)
                  (emote *nance* nil)
                  (setf *collision-on-p* t)
                  (setf *on-coffee-break* nil)))))


(defun game-over ()
  (setf *space-clamping-p* nil)
  (setf *collision-on-p* nil)
  (setf *input-mode* nil)
  (clear-keys-down)
  (harmony-simple:stop *current-track*)

  (dolist (p *pedestrians*) (setf (paused p) t))

  (let ((cop1 (make-instance 'human
                             :faces *cop-animation-faces*
                             :sheet *cop1-texture*))
        (cop2 (make-instance 'human
                             :faces *cop-animation-faces*
                             :sheet *cop2-texture*))
        (nance-y (y-pos *nance*)))

    (push cop1 *to-render-by-y*)
    (push cop2 *to-render-by-y*)
    (push cop1 *pedestrians*)
    (push cop2 *pedestrians*)

    (setf (pos cop1)
          (sdl2:make-rect (- (x-pos *nance*) 64) 620 64 128))
    (setf (pos cop2)
          (sdl2:make-rect (+ (x-pos *nance*) 64) 620 64 128))

    (push (sequencing (:at (sdl2:get-ticks))
            (grouping (:for 2600)
              (animating :the 'y-pos :of cop1 :to nance-y)
              (animating :the 'y-pos :of cop2 :to nance-y))
            (grouping (:for 2600)
              (animating :the 'y-pos :of cop1 :to 620)
              (animating :the 'y-pos :of cop2 :to 620)
              (animating :the 'y-pos :of *nance* :to 620))
            (take-action
             (lambda ()
               (setf *to-render-by-y* (delete *nance* *to-render-by-y*))
               (dolist (p *pedestrians*) (setf (paused p) nil))
               (end-fade-out))))
          *tweens*)))

(defun fade-in ()
  (setf *fading-out* (list 0 0 0 255))
  (push (animate *fading-out* 'cadddr 0
                 :start (sdl2:get-ticks) :duration 1000
                 :on-complete (lambda () (setf *fading-out* nil)))
        *tweens*))



(defun fade-out ()
  (setf *fading-out* (list 0 0 0 0))
  (push (animate *fading-out* 'cadddr 255 :start (sdl2:get-ticks) :duration 5000)
        *tweens*))

(defun end-fade-out ()
  (setf *fading-out* (list 0 0 0 0))
  (push (animate *fading-out* 'cadddr 200 :start (sdl2:get-ticks) :duration 10000)
        *tweens*))

;;;; HELPERS
(defun set-human-fps (n)
  (setf *human-frame-pause* (/ 1000 n)))

(defun reset-pedestrian (ped)
  (with-slots (already-asked pos walk-vec walk-speed) ped
    (setf already-asked nil)
    (setf (sdl2:rect-y pos) (random-y-pos))
    (setf (cdr walk-vec) 0)
    (if (cointoss)
        (progn
          (setf (sdl2:rect-x pos) -64)
          (setf (car walk-vec) walk-speed))
        (progn
          (setf (sdl2:rect-x pos) (+ 64 +window-width+))
          (setf (car walk-vec) (* -1 walk-speed))))
    (set-walk-face-by-walk-vec ped)))


(defun snap-hit-box-to (human hitbox)
  (setf (sdl2:rect-x hitbox) (x-pos human))
  (setf (sdl2:rect-y hitbox) (+ (y-pos human) 88)))

(defun get-frame-rect (human)
  (with-slots (faces face frame) human
    (let ((seq (funcall face faces)))
      (aref seq (mod frame (length seq))))))

(defun set-expression-rect (human)
  (setf (sdl2:rect-x *expression-rect*)
        (sdl2:rect-x (pos human)))
  (setf (sdl2:rect-y *expression-rect*)
        (- (sdl2:rect-y (pos human))
           (sdl2:rect-height *expression-rect*))))

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
  (+ +vert-min+ 64 (random (- +vert-max+ +vert-min+ 64))))

(defun choose-one (&rest options)
  (nth (random (length options)) options))

(defun any-p (arg &rest preds)
  (and preds
       (or (funcall (car preds) arg)
           (apply 'any-p (cons arg (cdr preds))))))

(defun all-p (arg &rest preds)
  (if preds
      (and (funcall (car preds) arg)
           (apply 'all-p (cons arg (cdr preds))))
      t))

(defun x-direction (person)
  (case (face person)
    ((facing-left walking-left) -1)
    ((facing-right walking-right) 1)
    (t 0)))

(defun y-direction (person)
  (case (face person)
    ((facing-up walking-up) -1)
    ((facing-down walking-down) 1)
    (t 0)))



;;; INPUT HANDLING

(defun action-key-pressed ()
  (cond
    ((and (not *on-coffee-break*)
          (in-front-of-door-p)
          (eql 'facing-up (face *nance*))
          (enough-for-food-p))
     (get-food!))
    ((and (not *on-coffee-break*)
          (in-front-of-door-p)
          (eql 'facing-up (face *nance*))
          (enough-for-coffee-p))
     (get-coffee!))
    (t
     (let-when (mark (find-if (lambda (ped)
                                (and (not (already-asked ped))
                                     (< (dist ped *nance*) 100)))
                              *pedestrians*))
       (stop-and-consider mark)))))

(defun clear-keys-down ()
  (setf *keys-down* (make-keys-down)))

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
    (case *input-mode*
      (:start
       (match-key key
         (:scancode-space (fade-into-game))))
      (:normal
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
                           (add-walk-hero-down))))))))

(defun handle-keyup (keysym)
  (let ((key (sdl2:scancode-value keysym)))
    (case *input-mode*
      (:normal
       (match-key key
         (:scancode-space (setf (keys-down-action *keys-down*) nil))
         (:scancode-left (setf (keys-down-left *keys-down*) nil)
                         (rem-walk-hero-left))

         (:scancode-right (setf (keys-down-right *keys-down*) nil)
                          (rem-walk-hero-right))

         (:scancode-up (setf (keys-down-up *keys-down*) nil)
                       (rem-walk-hero-up))

         (:scancode-down (setf (keys-down-down *keys-down*) nil)
                         (rem-walk-hero-down)))))))



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


;;; AUDIO

(defun play-track (track)
  (harmony-simple:stop *current-track*)
  (harmony-simple:resume track)
  (setf *current-track* track))

;;; GAME LOOP

;; (defun start-debug ()
;;   (bt:make-thread (lambda () (swank:create-server :port 4006 :dont-close t)))
;;   (start))


(defun start ()
  (setf +screen-sized-rect+ (sdl2:make-rect 0 0 +window-width+ +window-height+))
  (setf *status-meter-decoration-rect*  (sdl2:make-rect 0 0 48 48))
  (setf *sickness-rect*  (sdl2:make-rect 0 0 40 40))
  (setf *ped-hit-box* (sdl2:make-rect 0 0 64 32))
  (setf *nance-hit-box* (sdl2:make-rect 0 0 64 32))
  (setf *expression-rect*   (sdl2:make-rect 0 0 50 50))
  (setf *keys-down* (make-keys-down))

  (boot-meters)

  (setf *input-mode* :start)
  (setf *pedestrians* nil)
  (setf *to-render-by-y* nil)
  (setf *special-nance* t)
  (setf *space-clamping-p* t)
  (clear-keys-down)
  (setf (percent *money-meter*) 0)
  (setf (percent *stress-meter*) 0)
  (setf (percent *cold-meter*) 0)
  (setf *pedestrian-count* 4)
  (setf *collision-count* 0)

  (unwind-protect
       (sdl2:with-init (:everything)
         (sdl2:with-window (win :w 1024 :h 600 :title "The Price Of A Cup Of Coffee" :flags '(:shown))
           (sdl2:with-renderer (renderer win :flags '(:accelerated ))
             (boot-and-show-title renderer)
             (boot-up renderer)
             (sdl2:with-event-loop (:method :poll)
               (:keydown (:keysym keysym)
                         (if (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                             (sdl2:push-event :quit)
                             (handle-keydown keysym)))
               (:keyup (:keysym keysym)  (handle-keyup keysym))
               (:idle ()
                      (update :game (sdl2:get-ticks))
                      (unless (eql *input-mode* :start)
                        (render :game renderer))
                      (sdl2:delay +frame-delay+))
               (:quit () t)))))
    (free-assets)))

