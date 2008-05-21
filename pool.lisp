(in-package :pcall)

(defclass thread-pool ()
  ((open-p :initform t :accessor pool-open-p)
   (name :initarg :name :reader pool-name)
   (threads :initform () :accessor pool-threads)
   (tasks :initform (make-queue) :reader pool-tasks))
  (:documentation "Objects holding thread pools. Usually, there will
  be only one of these, in the top-level variable *thread-pool*."))

(defun make-thread-pool (n-threads &key (name "thread-pool"))
  "Create a new thread pool."
  (let ((pool (make-instance 'thread-pool :name name)))
    (dotimes (i n-threads)
      (push (worker-thread pool (format nil "~a-worker-~a" name (1+ i)))
            (pool-threads pool)))
    pool))

(defun close-thread-pool (pool &key max-wait)
  "Stop a thread pool from accepting any more tasks, try to wait for
  the existing tasks to finish running, and kill the treads. max-wait
  \(in seconds) can be given to raise an error when the tasks take too
  long to finish."
  (setf (pool-open-p pool) nil)
  (unwind-protect
    (loop :for waited :from 0 :by .2
          :until (and (zerop (queue-length (pool-tasks pool)))
                      (every (lambda (th) (eq (car th) :waiting))
                             (pool-threads pool)))
          :do (when (and max-wait (>= waited max-wait))
                (error "Max waiting time exceeded while closing thread pool."))
          :do (sleep .02))
    (dolist (th (pool-threads pool))
      (destroy-thread (cdr th)))))

(defun worker-thread (pool name)
  "The code running inside the pooled threads. Repeatedly tries to
take a task from the queue \(that is either not synchronised or whose
exclusive-lock is available), and handles it. The condition variable
is used when the task is joined while the thread is handling it -- it
will notify the joining thread when the task is done."
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

;; The global thread pool.
(defvar *thread-pool*)

(defun start-thread-pool (n-threads)
  "Set the global thread pool to a new pool."
  (when (boundp '*thread-pool*)
    (error "A thread pool has already been started."))
  (setf *thread-pool* (make-thread-pool n-threads))
  (values))

(defun stop-thread-pool (&key max-wait)
  "Close the global thread pool."
  (close-thread-pool *thread-pool* :max-wait max-wait)
  (makunbound '*thread-pool*))
