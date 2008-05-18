(in-package :pcall)

(defun pcall (thunk &optional exclusive)
  "Call a thunk in parallel. Returns a task that can be joined. When
an exclusive is given, the task will only run when no other tasks with
that exclusive are running."
  (let ((pool *thread-pool*))
    (unless (pool-open-p pool)
      (error "Can not enqueue a task to a closed pool."))
    (let ((task (make-instance 'task :thunk thunk :excl exclusive
                               :queue (pool-tasks pool))))
      (queue-push task (pool-tasks pool))
      task)))

(defmacro pprogn (&body body)
  "Shorthand for pcall."
  `(pcall (lambda () ,@body)))
