(in-package :pcall)

(defclass task ()
  ((thunk :initarg :thunk :reader task-thunk)
   (error :initform nil :accessor task-error)
   (values :accessor task-values)
   (lock :initform (make-lock) :reader task-lock)
   (status :initform :free :accessor task-status))
  (:documentation "A task is a piece of code that is scheduled to run
  in the thread pool. The status slot is used to make sure it is not
  run twice, once by a thread and once by the caller of join. The
  exclusive slot can be used to make sure no other tasks with the same
  exclusive run at the same moment."))

(defun handle-task (thread-condition task)
  "Execute a task \(as done by a thread), and store the result or
error in the task object. When a task's status is not :free, that
means the joiner has already started executing it, so this thread
should leave it alone."
  (unwind-protect
    (progn
      (with-lock-held ((task-lock task))
        (unless (eq (task-status task) :free)
          (return-from handle-task))
        (setf (task-status task) thread-condition))
      (handler-case
          (setf (task-values task) (multiple-value-list (funcall (task-thunk task))))
        (error (e) (setf (task-error task) e)))
      (with-lock-held ((task-lock task))
        (setf (task-status task) :done)
        (condition-notify thread-condition)))))

(defun join (task)
  "Join a task, meaning stop execution of the current thread until the
result of the task is available, and then return this result. When
this is called on a task that no thread is currently working on, the
current thread executes the task directly."
  (let ((mine nil))
    (with-lock-held ((task-lock task))
      (case (task-status task)
        (:done nil)
        (:free (setf mine t))
        (:joined (error "Joining an already joined task."))
        (t (condition-wait (task-status task) (task-lock task))))
      (setf (task-status task) :joined))
    (cond (mine (funcall (task-thunk task)))
          ((task-error task) (error (task-error task)))
          (t (values-list (task-values task))))))
