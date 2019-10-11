;;;; the-price-of-a-cup-of-coffee.lisp

(in-package #:the-price-of-a-cup-of-coffee)

(defvar *human-fps* 5)

(def-normal-class human ()
  (walk-vec (cons 0 0))
  pos
  sheet
  faces
  (face :facing-down)
  (frame 0)
  (next-frame-at 0))

(defmethod initialize-instance :after ((human human) &key)
  (with-slots (faces pos) human
    (let ((rect (aref (getf faces :facing-down) 0)))
      (setf pos
            (sdl2:make-rect 0 0
                            (sdl2:rect-width rect)
                            (sdl2:rect-height rect))))))

(defgeneric render (sprite renderer))
(defgeneric update (thing time))

(defmethod update ((human human) ticks)
  (with-slots (frame next-frame-at faces face walk-vec pos) human
    (incf (sdl2:rect-x pos) (car walk-vec))
    (incf (sdl2:rect-y pos) (cdr walk-vec))
    (when (<= next-frame-at ticks)
      (incf next-frame-at (/ 1000 *human-fps*))
      (setf next-frame-at (max next-frame-at ticks))
      (setf frame (mod (1+ frame) (length (getf faces face)))))))

(defmethod render ((human human) renderer)
  (with-slots (pos sheet faces face frame) human
    (sdl2:render-copy renderer sheet
                      :dest-rect pos
                      :source-rect (aref (getf faces face) frame))))


(def-normal-class pedestrian (human)
  (comfort-rad 60)
  (react-per-sec 1)
  (anger 0.1)
  (kindness 0.02)
  (generosity 0.25)
  (vulnerability 3))

(def-normal-class hero (human)
  (stress 0)
  (money 0)
  (coldness 0)
  (sick-p nil)
  (relax-rate 1))


(defvar *nance* nil)

(defun boot-up (renderer)
  (setf *nance-tile-defs* (make-source-rects *nance-tile-defs*))

  (with-surface-from-file (surf +nance-sheet-image+)
    (setf *nance*
          (make-instance 'hero
                         :sheet (sdl2:create-texture-from-surface renderer surf)
                         :faces (create-sprite-faces *nance-tile-defs*)))))


(defparameter +frame-delay+ (round (/ 1000 60)))

(defparameter +action-key+ :scancode-space)

(defun action-key-pressed ()
  (print "Action"))


(defun walking-face (dir)
  (case dir
    (:left :walking-left)
    (:right :walking-right)
    (:up :walking-up)
    (:down :walking-down)))

(defun standing-face (dir)
  (case dir
    (:left :facing-left)
    (:right :facing-right)
    (:up :facing-up)
    (:down :facing-down)))

;; (defun facing-p (human dir)
;;   (equal dir 
;;          (case (face human)
;;            ((:facing-down :walking-down) :down)
;;            ((:facing-up :walking-up) :up)
;;            ((:facing-left :walking-left) :left)
;;            ((:facing-right :walking-right) :right))))

(defun walk-hero (dir)
  (unless (equal (walking-face dir) (face *nance*))
    (setf (face *nance*) (walking-face dir))
    (setf (frame *nance*) 0)
    (setf (walk-vec *nance*)
          (case dir
            (:right (cons 6 0))
            (:left (cons -6 0))
            (:up (cons 0 -6))
            (:down (cons 0 6))))))

(defun stop-hero (dir)
  (setf (face *nance*)
        (standing-face dir))
  (setf (frame *nance*) 0)
  (setf (walk-vec *nance*) (cons 0 0)))


(defun handle-keydown (keysym)
  (let ((key (sdl2:scancode-value keysym)))
    (match-key key
               (:scancode-left (walk-hero :left))
               (:scancode-right (walk-hero  :right))
               (:scancode-up (walk-hero :up))
               (:scancode-down (walk-hero :down)))))


(defun handle-keyup (keysym)
  (let ((key (sdl2:scancode-value keysym)))
    (match-key key
      (+action-key+ (action-key-pressed))
      (:scancode-left (stop-hero :left))
      (:scancode-right (stop-hero  :right))
      (:scancode-up (stop-hero :up))
      (:scancode-down (stop-hero :down)))))


(defmethod render ((game (eql :game)) renderer)
  (sdl2:set-render-draw-color renderer 255 255 255 255)
  (sdl2:render-clear renderer)
  (render *nance* renderer)
  (sdl2:render-present renderer))


(defun main ()
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
                 (free-assets)
                 t))))))

(defun free-assets ()
  (setf (sheet *nance*) nil))

