(in-package :pcall)

(defclass task ()
  ((thunk :initarg :thunk :reader task-thunk)
   (error :initform nil :accessor task-error)
   (values :accessor task-values)
   (lock :initform (make-lock) :reader task-lock)
   (status :initform :free :accessor task-status)
   (wait-condition :initform nil :accessor task-condition))
  (:documentation "A task is a piece of code that is scheduled to run
  in the thread pool. The status slot is used to make sure it is not
  run twice, once by a thread and once by the caller of join. The
  exclusive slot can be used to make sure no other tasks with the same
  exclusive run at the same moment."))

(defun execute-task (task)
  "Execute a task, and store the result or error in the task object.
When a task's status is not :free, that means the joiner has already
started executing it, so this thread should leave it alone."
  (handler-case
      (setf (task-values task) (multiple-value-list (funcall (task-thunk task))))
    (error (e) (setf (task-error task) e)))
  (with-lock-held ((task-lock task))
    (setf (task-status task) :done)
    (when (task-condition task)
      (condition-notify (task-condition task)))))

(defun join (task)
  "Join a task, meaning stop execution of the current thread until the
result of the task is available, and then return this result. When
this is called on a task that no thread is currently working on, the
current thread executes the task directly."
  (let ((mine nil))
    (with-lock-held ((task-lock task))
      (ecase (task-status task)
        (:free (setf mine t (task-status task) :running))
        (:running (setf (task-condition task) (make-condition-variable))
                  (condition-wait (task-condition task) (task-lock task)))
        (:done nil)))
    (when mine (execute-task task))
    (if (task-error task)
        (error (task-error task))
        (values-list (task-values task)))))
