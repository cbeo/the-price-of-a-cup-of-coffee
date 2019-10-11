;;;; the-price-of-a-cup-of-coffee.lisp

(in-package #:the-price-of-a-cup-of-coffee)

(defun make-keyword-symbol (s)
  "Makes a keyword from a string or symbol."
  (let ((s (format nil "~a" s)))
    (read-from-string
     (format nil ":~a"
             (substitute #\- #\Space s)))))


(defmacro def-normal-class (name super &rest slots)
  "Defines a class with the given name and slots, with accessors and initargs for each slot."
  `(defclass ,name ,super
     (,@(loop :for slot :in slots
              :when (consp slot)
                :collect (list (car slot)
                               :accessor (car slot)
                               :initform (cadr slot)
                               :initarg (make-keyword-symbol (car slot)))
              :else
                :collect (list slot
                               :accessor slot
                               :initform nil
                               :initarg (make-keyword-symbol slot))))))


(def-normal-class pedestrian ()
  (walk-vec (list 2 0))
  (comfort-rad 60)
  (react-per-sec 1)
  (anger 0.1)
  (kindness 0.02)
  (generosity 0.25)
  (vulnerability 3))


(def-normal-class ()
  (stress 0)
  (money 0)
  (coldness 0)
  (sick-p nil)
  (speed 5)
  (relax-rate 1)


(defun main ()
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :w 800 :h 600 :title "The Price Of A Cup Of Coffee" :flags '(:shown))
      (sdl2:with-renderer (rndr win :flags '(:accelerated))
        (sdl2:with-event-loop (:method :poll)

          (:keydown (:keysym keysym)
                    (if (sdl2:scancode= (sdl2:scancode-value keysym) :scancode-escape)
                        (sdl2:push-event :quit)))
          (:idle ())

          (:quit () t))))))


