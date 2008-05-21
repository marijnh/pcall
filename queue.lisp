(cl:defpackage :pcall-queue
  (:use :cl :bordeaux-threads)
  (:export #:make-queue
           #:queue-push
           #:queue-pop #:queue-wait
           #:queue-length #:queue-empty-p))

(cl:in-package :pcall-queue)

;;; A thread-safe wait queue.

(defclass queue ()
  ((lock :initform (make-lock) :reader queue-lock)
   (condition :initform (make-condition-variable) :reader queue-condition)
   (front :initform nil :accessor queue-front)
   (back :initform nil :accessor queue-back)))

(defstruct node val next prev)

(defun make-queue ()
  "Create an empty queue."
  (make-instance 'queue))

(defun queue-push (elt queue)
  "Push an element onto the back of a queue."
  (with-lock-held ((queue-lock queue))
    (let* ((back (queue-back queue))
           (node (make-node :val elt :prev back :next nil)))
      (setf (queue-back queue) node)
      (cond (back (setf (node-next back) node))
            (t (setf (queue-front queue) node))))
    (condition-notify (queue-condition queue)))
  (values))

(defun queue-do-pop (queue)
  (let ((node (queue-front queue)))
    (if node
        (progn
          (setf (queue-front queue) (node-next node))
          (unless (node-next node)
            (setf (queue-back queue) nil))
          (values (node-val node) t))
        (values nil nil))))

(defun queue-pop (queue)
  "Pop an element from the front of a queue. Returns immediately,
returning nil if the queue is empty, and a second value indicating
whether anything was popped."
  (with-lock-held ((queue-lock queue))
    (queue-do-pop queue)))

(defun queue-wait (queue)
  "Pop an element from the front of a queue. Causes a blocking wait
when no elements are available."
  (with-lock-held ((queue-lock queue))
    (loop (multiple-value-bind (elt found) (queue-do-pop queue)
            (when found (return elt)))
          (condition-wait (queue-condition queue) (queue-lock queue)))))

(defun queue-length (queue)
  "Find the length of a queue."
  (with-lock-held ((queue-lock queue))
    (loop :for node := (queue-front queue) :then (node-next node)
          :for l :from 0
          :while node
          :finally (return l))))

(defun queue-empty-p (queue)
  "Test whether a queue is empty."
  (null (queue-front queue)))
