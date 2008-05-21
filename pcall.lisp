(in-package :pcall)

(defun pcall (thunk)
  "Call a thunk in parallel. Returns a task that can be joined. When
an exclusive is given, the task will only run when no other tasks with
that exclusive are running."
  (let ((pool *thread-pool*))
    (unless (pool-open-p pool)
      (error "Can not enqueue a task to a closed pool."))
    (let ((task (make-instance 'task :thunk thunk)))
      (queue-push task (pool-tasks pool))
      task)))

(defmacro prun (&body body)
  "Shorthand for pcall."
  `(pcall (lambda () ,@body)))
