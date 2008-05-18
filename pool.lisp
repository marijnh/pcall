(in-package :pcall)

(defclass thread-pool ()
  ((open-p :initform t :accessor pool-open-p)
   (name :initarg :name :reader pool-name)
   (threads :initform () :accessor pool-threads)
   (tasks :initform (make-queue) :reader pool-tasks)))

(defun make-thread-pool (n-threads &key (name "thread-pool"))
  (let ((pool (make-instance 'thread-pool :name name)))
    (dotimes (i n-threads)
      (push (worker-thread pool (format nil "~a-worker-~a" name (1+ i)))
            (pool-threads pool)))
    pool))

(defun close-thread-pool (pool &key max-wait)
  (setf (pool-open-p pool) nil)
  (loop :for waited :from 0 :by .2
        :until (or (and max-wait (>= waited max-wait))
                   (and (zerop (queue-length (pool-tasks pool)))
                        (every (lambda (th) (eq (car th) :waiting))
                               (pool-threads pool))))
        :do (sleep .2))
  (dolist (th (pool-threads pool))
    (destroy-thread (cdr th))))

(defun worker-thread (pool name)
  (let ((thread-condition (make-condition-variable))
        (status (cons :waiting nil)))
    (flet ((run ()
             (loop
               (let ((task (queue-wait (pool-tasks pool))))
                 (setf (car status) :running)
                 (handle-task thread-condition task)
                 (setf (car status) :waiting)))))
      (setf (cdr status) (make-thread #'run :name name))
      status)))

(defvar *thread-pool*)

(defun start-thread-pool (n-threads)
  (when (boundp '*thread-pool*)
    (error "A thread pool has already been started."))
  (setf *thread-pool* (make-thread-pool n-threads))
  (values))

(defun stop-thread-pool (&key max-wait)
  (close-thread-pool *thread-pool* :max-wait max-wait)
  (makunbound '*thread-pool*))
