(in-package :pcall)

(defun pcall (thunk)
  "Call a thunk in parallel. Returns a task that can be joined. When
an exclusive is given, the task will only run when no other tasks with
that exclusive are running."
  (let ((task (make-instance 'task :thunk thunk)))
    (queue-push task (pool-queue *pool*))
    (unless (pool-threads *pool*)
      (audit-thread-pool))
    task))

(defmacro pexec (&body body)
  "Shorthand for pcall."
  `(pcall (lambda () ,@body)))

(defmacro plet ((&rest bindings) &body body)
  (let ((syms (mapcar (lambda (x) (gensym (string (car x)))) bindings)))
    `(let ,(loop :for (nil val) :in bindings
                 :for sym :in syms
                 :collect `(,sym (pexec ,val)))
       (symbol-macrolet ,(loop :for (var nil) :in bindings
                               :for sym :in syms
                               :collect `(,var (join ,sym)))
         ,@body))))
