(in-package :pcall)

(defun pcall (thunk &optional (pool *thread-pool*))
  (unless (pool-open-p pool)
    (error "Can not enqueue a task to a closed pool."))
  (let ((task (make-instance 'task :thunk thunk)))
    (queue-push task (pool-tasks pool))
    task))

(defmacro pprogn (&body body)
  `(pcall (lambda () ,@body)))

(defmacro pprogn* (pool &body body)
  `(pcall (lambda () ,@body) ,pool))
