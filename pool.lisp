(in-package :pcall)

(defvar *thread-pool* ())
(defparameter *thread-pool-size* 3)
(defvar *thread-pool-lock* (make-lock))

(define-condition stop-running () ())

(defun audit-thread-pool ()
  "Make sure the thread pool holds *thread-pool-size* live threads."
  (with-lock-held (*thread-pool-lock*)
    (setf *thread-pool* (remove-if-not #'thread-alive-p *thread-pool*))
    (let ((threads (length *thread-pool*)))
      (cond ((< threads *thread-pool-size*)
             (dotimes (i (- *thread-pool-size* threads)) (spawn-thread)))
            ((> threads *thread-pool-size*)
             (loop :for i :from 0 :below (- threads *thread-pool-size*)
                   :for thread :in *thread-pool*
                   :do (stop-thread thread)))))))

(let ((counter 0))
  (defun spawn-thread ()
    (incf counter)
    (push (make-thread (worker-thread) :name (format nil "pcall-worker-~a" counter))
          *thread-pool*)))

(defun stop-thread (thread)
  (when (thread-alive-p thread)
    (interrupt-thread thread (lambda () (signal 'stop-running)))))

(defun finish-tasks ()
  (let (old-pool)
    (with-lock-held (*thread-pool-lock*)
      (setf old-pool *thread-pool*
            *thread-pool* nil))
    (loop :until (queue-empty-p *task-queue*)
          :do (sleep .05))
    (mapc #'stop-thread old-pool)
    (loop :while (some #'thread-alive-p old-pool)
          :do (sleep .05))))

(defun thread-pool-size ()
  *thread-pool-size*)

(defun (setf thread-pool-size) (size)
  (setf *thread-pool-size* size)
  (audit-thread-pool))

(defun worker-thread ()
  "The code running inside the pooled threads. Repeatedly tries to
take a task from the queue, and handles it."
  (lambda ()
    (let ((stop nil))
      (handler-bind ((stop-running (lambda (c) (declare (ignore c)) (setf stop t))))
        (loop :until stop
              :do (let ((task (handler-case (queue-wait *task-queue*)
                                (stop-running () (setf stop t) nil))))
                    (when task
                      (with-lock-held ((task-lock task))
                        (if (eq (task-status task) :free)
                            (setf (task-status task) :running)
                            (setf task nil))))
                    (when task (execute-task task))))))))
