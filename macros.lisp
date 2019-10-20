(in-package :the-price-of-a-cup-of-coffee)

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

(defmacro let-cond (&body forms)
  (let ((tmp-var (gensym)))
    `(let (,tmp-var)
       (cond
         ,@(loop :for (var test . body) :in forms
                 :if (eq var t)
                   :collect (list* t (cons test body))
                 :else
                   :collect `((setf ,tmp-var ,test)
                              (let ((,var ,tmp-var))
                                ,@body)))))))
(defmacro match-key (key &body clauses)
  "Each clause is of the form (:scancode-xxx expr1 expr2 ...)"
  `(cond ,@(loop :for (scancode . actions) :in clauses
                 :collect `((sdl2:scancode= ,key ,scancode) ,@actions))))

(defmacro let-when ((var test) &body body)
  `(let ((,var ,test))
     (when ,var ,@body)))

(defmacro let-if ((var test) then &optional else)
  `(let ((,var ,test))
     (if ,var ,then ,else)))


(defmacro $ (f &rest args)
  (let* ((new-args (loop :for a :in args :when (eql a '_) :collect (gensym)))
         (copy-new (copy-seq new-args))
         (call-args (loop :for a :in args
                          :when (eql a '_) :collect (pop copy-new)
                            :else :collect a)))
    `(lambda ,new-args (funcall ,f ,@call-args))))

(defmacro with-surface-from-file ((var path) &body body)
  `(let ((,var (sdl2-image:load-image ,path)))
     (unwind-protect
          (progn ,@body)
       (sdl2:free-surface ,var))))


(defmacro with-surface ((var surf) &body body)
  `(let ((,var ,surf))
     (unwind-protect
          (progn ,@body)
       (sdl2:free-surface ,var))))
