(in-package :pcall)

(defclass task ()
  ((thunk :initarg :thunk :reader task-thunk)
   (error :initform nil :accessor task-error)
   (values :accessor task-values)
   (lock :initform (make-lock) :reader task-lock)
   (wait-locks :initform nil :accessor task-wait-locks)
   (status :initform :free :accessor task-status)
   (waiting :initform nil :accessor task-waiting))
  (:documentation "A task is a piece of code that is scheduled to run
  in the thread pool."))

(defun with-locks (locks thunk)
  (if locks
      (with-lock-held ((car locks)) (with-locks (cdr locks) thunk))
      (funcall thunk)))

(defun execute-task (task)
  "Execute a task, and store the result or error in the task object.
When a task's status is not :free, that means the joiner has already
started executing it, so this thread should leave it alone."
  (handler-case
      (setf (task-values task) (multiple-value-list (funcall (task-thunk task))))
    (error (e) (setf (task-error task) e)))
  (with-lock-held ((task-lock task))
    (setf (task-status task) :done)
    (with-locks (task-wait-locks task)
      (lambda () (mapc #'condition-notify (task-waiting task))))))

(defun join (task)
  "Join a task, meaning stop execution of the current thread until the
result of the task is available, and then return this result. When
this is called on a task that no thread is currently working on, the
current thread executes the task directly."
  (let ((mine nil))
    (with-lock-held ((task-lock task))
      (ecase (task-status task)
        (:free (setf mine t (task-status task) :running))
        (:running (let ((wait (make-condition-variable)))
                    (push wait (task-waiting task))
                    (loop :until (eq (task-status task) :done)
                          :do (condition-wait wait (task-lock task)))))
        (:done nil)))
    (when mine (execute-task task))
    (if (task-error task)
        (error (task-error task))
        (values-list (task-values task)))))

(defun join-one (&rest tasks)
  "Returns the result, and the task object, of the first task among
the arguments to finish computing."
  (flet ((join-if-done (task)
           (when (eq (task-status task) :done)
             (if (task-error task)
                 (error (task-error task))
                 (return-from join-one (values (car (task-values task)) task))))))
    (let ((notifier (make-condition-variable))
          (our-lock (make-lock)))
      (with-lock-held (our-lock)
        (dolist (task tasks) ;; ensure that tasks can't finish unless they have our lock
          (with-lock-held ((task-lock task))
            (join-if-done task)
            (push notifier (task-waiting task))
            (push our-lock (task-wait-locks task))))
        (loop
         (condition-wait notifier our-lock)
         (dolist (task tasks)
           (with-lock-held ((task-lock task))
             (join-if-done task))))))))

(defun done-p (task)
  (eq (task-status task) :running))
