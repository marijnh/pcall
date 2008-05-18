(cl:defpackage :pcall-queue
  (:use :cl :bordeaux-threads)
  (:export #:make-queue
           #:queue-push #:queue-push-back
           #:queue-pop #:queue-wait
           #:queue-length #:queue-empty-p))

(cl:in-package :pcall-queue)

;;; A thread-safe wait queue. Uses front and back lists for amortised
;;; O(1) popping cost and implementation simplicity.

(defclass queue ()
  ((lock :initform (make-lock) :reader queue-lock)
   (condition :initform (make-condition-variable) :reader queue-condition)
   (front :initform () :accessor queue-front)
   (back :initform () :accessor queue-back)))

(defun make-queue ()
  "Create an empty queue."
  (make-instance 'queue))

(defun queue-push (elt queue)
  "Push an element to the front of a queue. \(Will be popped last.)"
  (with-lock-held ((queue-lock queue))
    (push elt (queue-front queue)))
  (condition-notify (queue-condition queue))
  (values))

(defun queue-push-back (elt queue)
  "Push an element to the back of a queue. \(Will be popped first.)"
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
  "Pop an element from the back of a queue. Returns immediately,
returning nil if the queue is empty, and a second value indicating
whether anything was popped."
  (with-lock-held ((queue-lock queue))
    (queue-do-pop queue)))

(defun queue-wait (queue)
  "Pop an element from the back of a queue. Causes a blocking wait
when no elements are available."
  (with-lock-held ((queue-lock queue))
    (loop
       (multiple-value-bind (elt found) (queue-do-pop queue)
         (when found (return elt)))
       (condition-wait (queue-condition queue) (queue-lock queue)))))

(defun queue-length (queue)
  "Find the length of a queue."
  (with-lock-held ((queue-lock queue))
    (+ (length (queue-front queue)) (length (queue-back queue)))))

(defun queue-empty-p (queue)
  (and (null (queue-front queue)) (null (queue-back queue))))
