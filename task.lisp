(in-package :pcall)

(defclass task ()
  ((thunk :initarg :thunk :reader task-thunk)
   (error :initform nil :accessor task-error)
   (values :accessor task-values)
   (lock :initform (make-lock) :reader task-lock)
   (owner :initform nil :accessor task-owner)))

(defun handle-task (thread-condition task)
  (with-lock-held ((task-lock task))
    (if (task-owner task)
        (setf task nil)
        (setf (task-owner task) thread-condition)))
  (when task
    (handler-case
        (setf (task-values task) (multiple-value-list (funcall (task-thunk task))))
      (error (e) (setf (task-error task) e)))
    (setf (task-owner task) :done)
    (condition-notify thread-condition)))

(defun join (task)
  (with-lock-held ((task-lock task))
    (cond ((eq (task-owner task) :done) nil)
          ((eq (task-owner task) nil) (setf (task-owner task) :me))
          (t (condition-wait (task-owner task) (task-lock task)))))
  (cond ((eq (task-owner task) :me) (funcall (task-thunk task)))
        ((task-error task) (error (task-error task)))
        (t (values-list (task-values task)))))
