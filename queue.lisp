(cl:defpackage :pcall-queue
  (:use :cl :bordeaux-threads)
  (:export #:make-queue
           #:queue-push #:queue-push-back
           #:queue-pop #:queue-wait
           #:queue-length))

(cl:in-package :pcall-queue)

(defclass queue ()
  ((lock :initform (make-lock) :reader queue-lock)
   (condition :initform (make-condition-variable) :reader queue-condition)
   (front :initform () :accessor queue-front)
   (back :initform () :accessor queue-back)))

(defun make-queue ()
  (make-instance 'queue))

(defun queue-push (queue elt)
  (with-lock-held ((queue-lock queue))
    (push elt (queue-front queue)))
  (condition-notify (queue-condition queue))
  (values))

(defun queue-push-back (queue elt)
  (with-lock-held ((queue-lock queue))
    (push elt (queue-back queue)))
  (condition-notify (queue-condition queue))
  (values))

(defun queue-do-pop (queue)
  (when (null (queue-back queue))
    (setf (queue-back queue) (nreverse (queue-front queue))
          (queue-front queue) nil))
  (if (null (queue-back queue))
      (values nil nil)
      (values (pop (queue-back queue)) t)))

(defun queue-pop (queue)
  (with-lock-held ((queue-lock queue))
    (queue-do-pop queue)))

(defun queue-wait (queue)
  (with-lock-held ((queue-lock queue))
    (loop
       (multiple-value-bind (elt found) (queue-do-pop queue)
         (when found (return elt)))
       (condition-wait (queue-condition queue) (queue-lock queue)))))

(defun queue-length (queue)
  (with-lock-held ((queue-lock queue))
    (+ (length (queue-front queue)) (length (queue-back queue)))))
