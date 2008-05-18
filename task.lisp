(in-package :pcall)

(defclass task ()
  ((thunk :initarg :thunk :reader task-thunk)
   (queue :initarg :queue :reader task-queue)
   (error :initform nil :accessor task-error)
   (values :accessor task-values)
   (lock :initform (make-lock) :reader task-lock)
   (status :initform :free :accessor task-status)
   (exclusive :initarg :excl :reader task-excl)))

(defun handle-task (thread-condition task)
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
        (condition-notify thread-condition)))
    (release-task task)))

(defun join (task)
  (with-lock-held ((task-lock task))
    (cond ((eq (task-status task) :done) nil)
          ((eq (task-status task) :free) (setf (task-status task) :mine))
          (t (condition-wait (task-status task) (task-lock task)))))
  (cond ((eq (task-status task) :mine)
         (wait-for-task task)
         (unwind-protect (funcall (task-thunk task))
           (release-task task)))
        ((task-error task) (error (task-error task)))
        (t (values-list (task-values task)))))

;; Mutually exclusive tasks.

(defclass exclusive ()
  ((lock :initform (make-lock) :reader excl-lock)
   (condition :initform (make-condition-variable) :reader excl-condition)
   (taken-p :initform nil :accessor excl-taken-p)))

(defun claim-task (task)
  (let ((excl (task-excl task)))
    (or (not excl)
        (with-lock-held ((excl-lock excl))
          (and (not (excl-taken-p excl)) 
               (setf (excl-taken-p excl) t))))))

(defun wait-for-task (task)
  (let ((excl (task-excl task)))
    (when excl
      (with-lock-held ((excl-lock excl))
        (loop :while (excl-taken-p excl)
              :do (condition-wait (excl-condition excl) (excl-lock excl)))
        (setf (excl-taken-p excl) t)))))
        
(defun release-task (task)
  (let ((excl (task-excl task)))
    (when excl
      (setf (excl-taken-p excl) nil)
      (condition-notify (excl-condition excl))
      (queue-notify (task-queue task)))
    excl))

(defun make-exclusive ()
  (make-instance 'exclusive))
