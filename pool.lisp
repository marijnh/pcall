(in-package :pcall)

(defstruct pool
  (threads ())
  (size 3)
  (lock (make-lock))
  (env nil)
  (queue (make-queue)))

(defvar *pool* (make-pool))

(define-condition stop-running ()
  ((at-once-p :initarg :at-once-p :reader stop-at-once-p)))

(defun audit-thread-pool ()
  "Make sure the thread pool holds the correct amount of live threads."
  (with-lock-held ((pool-lock *pool*))
    (setf (pool-threads *pool*) (remove-if-not #'thread-alive-p (pool-threads *pool*)))
    (let ((threads (length (pool-threads *pool*)))
          (size (pool-size *pool*)))
      (cond ((< threads size)
             (dotimes (i (- size threads)) (spawn-thread *pool*)))
            ((> threads size)
             (loop :for i :from 0 :below (- threads size)
                   :for thread :in (pool-threads *pool*)
                   :do (stop-thread thread t)))))))

(let ((counter 0))
  (defun spawn-thread (pool)
    (flet ((run ()
             (if (pool-env pool)
                 (funcall (pool-env pool) (lambda () (worker-thread (pool-queue pool))))
                 (worker-thread (pool-queue pool)))))
      (push (make-thread #'run :name (format nil "pcall-worker-~a" (incf counter)))
            (pool-threads pool)))))

(defun stop-thread (thread at-once-p)
  (when (thread-alive-p thread)
    (interrupt-thread thread (lambda () (signal 'stop-running :at-once-p at-once-p)))))

(defun finish-tasks ()
  (let (old-pool)
    (with-lock-held ((pool-lock *pool*))
      (setf old-pool (pool-threads *pool*)
            (pool-threads *pool*) nil))
    (dolist (th old-pool) (stop-thread th nil))
    (loop :while (some #'thread-alive-p old-pool)
          :do (sleep .05))))

(defun thread-pool-size ()
  (pool-size *pool*))

(defun (setf thread-pool-size) (size)
  (setf (pool-size *pool*) size)
  (audit-thread-pool))

(defun set-worker-environment (wrapper)
  (setf (pool-env *pool*) wrapper)
  (finish-tasks))

(defun worker-thread (queue)
  "The code running inside the pooled threads. Repeatedly tries to
take a task from the queue, and handles it."
  (let ((stop nil))
    (flet ((stop-running (condition)
             (unless (eq stop :now)
               (setf stop (if (stop-at-once-p condition) :now :when-empty)))))
      (handler-bind ((stop-running #'stop-running))
        (loop :until (or (eq stop :now)
                         (and (eq stop :when-empty) (queue-empty-p queue)))
              :do (let ((task (handler-case (queue-wait queue)
                                (stop-running (c) (stop-running c) nil))))
                    (when task
                      (with-lock-held ((task-lock task))
                        (if (eq (task-status task) :free)
                            (setf (task-status task) :running)
                            (setf task nil))))
                    (when task (execute-task task))))))))

(defmacro with-local-thread-pool ((&key (size '(pool-size *pool*)) (on-unwind :wait)
                                        (worker-environment '(pool-env *pool*))) &body body)
  "Run body with a fresh thread pool. If on-unwind is :wait, it will
wait for all tasks to finish before returning. If it is :leave, the
form will return while threads are still working. If it is :stop
or :destroy, the threads will be stopped at the end of the body.
With :stop, they will first finish their current task (if any),
with :destroy, they will be brutally destroyed and might leak
resources, leave stuff in inconsistent state, etc."
  `(let ((*pool* (make-pool :size ,size :env ,worker-environment)))
     (unwind-protect (progn ,@body)
       ,(ecase on-unwind
          (:wait '(finish-tasks))
          ((:leave :stop) `(dolist (th *thread-pool*) (stop-thread th ,(eq on-unwind :stop))))
          (:destroy '(dolist (th *thread-pool*) (destroy-thread th)))))))
