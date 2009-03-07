(in-package :pcall)

(defvar *thread-pool* ())
(defparameter *thread-pool-size* 3)
(defvar *thread-pool-lock* (make-lock))

(define-condition stop-running ()
  ((at-once-p :initarg :at-once-p :reader stop-at-once-p)))

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
                   :do (stop-thread thread t)))))))

(let ((counter 0))
  (defun spawn-thread ()
    (incf counter)
    (push (make-thread 'worker-thread :name (format nil "pcall-worker-~a" counter))
          *thread-pool*)))

(defun stop-thread (thread at-once-p)
  (when (thread-alive-p thread)
    (interrupt-thread thread (lambda () (signal 'stop-running :at-once-p at-once-p)))))

(defun finish-tasks ()
  (let (old-pool)
    (with-lock-held (*thread-pool-lock*)
      (setf old-pool *thread-pool*
            *thread-pool* nil))
    (dolist (th old-pool) (stop-thread th nil))
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
  (let ((stop nil))
    (flet ((stop-running (condition)
             (unless (eq stop :now)
               (setf stop (if (stop-at-once-p condition) :now :when-empty)))))
      (handler-bind ((stop-running #'stop-running))
        (loop :until (or (eq stop :now)
                         (and (eq stop :when-empty) (queue-empty-p *task-queue*)))
              :do (let ((task (handler-case (queue-wait *task-queue*)
                                (stop-running (c) (stop-running c) nil))))
                    (when task
                      (with-lock-held ((task-lock task))
                        (if (eq (task-status task) :free)
                            (setf (task-status task) :running)
                            (setf task nil))))
                    (when task (execute-task task))))))))

(defmacro with-local-thread-pool ((&key (size *thread-pool-size*) (on-unwind :wait)) &body body)
  "Run body with a fresh thread pool. If on-unwind is :wait, it will
wait for all tasks to finish before returning. If it is :leave, the
form will return while threads are still working. If it is :stop
or :destroy, the threads will be stopped at the end of the body.
With :stop, they will first finish their current task (if any),
with :destroy, they will be brutally destroyed and might leak
resources, leave stuff in inconsistent state, etc."
  `(let ((*thread-pool* ())
	 (*thread-pool-size* ,size)
	 (*thread-pool-lock* (make-lock)))
     (unwind-protect (progn ,@body)
       ,(ecase on-unwind
          (:wait '(finish-tasks))
          ((:leave :stop) `(dolist (th *thread-pool*) (stop-thread th ,(eq on-unwind :stop))))
          (:destroy '(dolist (th *thread-pool*) (destroy-thread th)))))))
