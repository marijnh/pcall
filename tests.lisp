(cl:defpackage :pcall-tests
  (:use :cl :fiveam :pcall :pcall-queue :bordeaux-threads))

(cl:in-package :pcall-tests)

(def-suite :pcall)
(in-suite :pcall)

;; Queue-related tests

(test queue-sanity
  (let ((q (make-queue)))
    (queue-push 5 q)
    (queue-push 10 q)
    (is (=  2 (queue-length q)))
    (is (equal 5 (queue-pop q)))
    (multiple-value-bind (e f) (queue-pop q)
      (is (equal 10 e))
      (is (eq t f)))
    (is (eq nil (nth-value 1 (queue-pop q))))))

(defun wait-until-queue-empty (q)
  (loop :until (queue-empty-p q) :do (sleep .01))
  (sleep .05)) ; <- crummy, still a possible race condition

(test queue-wait
  (let ((q (make-queue))
        (out nil))
    (make-thread (lambda () (push (queue-wait q) out)))
    (is (eq nil out))
    (queue-push 10 q)
    (wait-until-queue-empty q)
    (is (equal '(10) out))))

(test queue-concurrency
  (let ((q (make-queue))
        (l (make-lock))
        (out nil))
    (dotimes (i 1000)
      (queue-push i q))
    (dotimes (i 10)
      (make-thread (lambda ()
                     (loop (multiple-value-bind (val found) (queue-pop q)
                             (unless found (return))
                             (with-lock-held (l) (push val out))
                             (sleep .001))))))
    (wait-until-queue-empty q)
    (is (= 1000 (length out)))
    (is (every (lambda (x) (member x out)) (loop :for i :from 0 :below 1000 :collect i)))))

;; PCall tests

(defmacro with-thread-pool (&body body)
  `(unwind-protect (progn ,@body)
     (finish-tasks)))

(test sanity
  (with-thread-pool
    (is (equal '(1 2 3) (join (pexec (list 1 2 3)))))
    (let ((task (pexec (+ 4 2))))
      (sleep .01)
      (is (= 6 (join task))))))

(test stress
  (flet ((compute ()
           (loop :for i :from 0 :below 100000
                 :sum (* i i))))
    (with-thread-pool
      (let ((tasks (loop :for i :from 0 :below 1000 :collect (pcall #'compute)))
            (answer (compute)))
        (sleep .05)
        (is (every (lambda (tsk) (= (join tsk) answer)) tasks))))))

(test multi-join
  (with-thread-pool
    (let* ((task (pexec (sleep .1) :ok))
           (joiners (loop :for i :from 0 :below 10
                          :collect (pexec (join task)))))
      (sleep .01)
      (is (every (lambda (tsk) (eq (join tsk) :ok)) joiners)))))

(test plet
  (with-thread-pool
    (plet ((x (list 1 2))
           (y (list 3 4)))
      (sleep .01)
      (is (equal '(1 2 3 4) (append x y))))))

(test delayed-signal
  (with-thread-pool
    (let ((task (pexec (error "Wrong!"))))
      (sleep .01)
      (signals simple-error (join task)))))

(test local-pool
  (let ((outer-size (thread-pool-size))
        (switch :off))
    (with-local-thread-pool (:size 5 :on-unwind :wait)
      (is (= 5 (thread-pool-size)))
      (pexec (sleep .2) (setf switch :on)))
    (is (eq :on switch))
    (is (= outer-size (thread-pool-size)))))

(defvar *x*)

(test enviroment
  (let ((*x* :a))
    (set-worker-environment (lambda (f) (let ((*x* :b)) (funcall f))))
    (plet ((x *x*))
      (sleep .01)
      (is (equal '(:a . :b) (cons *x* x))))
    (set-worker-environment nil)))

(test local-environment
  (let ((*x* :c))
    (with-local-thread-pool (:worker-environment (lambda (f) (let ((*x* :d)) (funcall f))))
      (plet ((x *x*))
        (sleep .01)
        (is (equal '(:c . :d) (cons *x* x)))))))

(test select-one-random
  (is (member (join (select-one (pexec (sleep (random 0.2)) 1)
                                (pexec (sleep (random 0.2)) 2)
                                (pexec (sleep (random 0.2)) 3))) '(1 2 3))))

(test select-one-always
  (is (= 2 (join (select-one (pexec (sleep 0.05) 1) (pexec 2))))))

(test select-one-all-tasks-done
  (flet ((make-done-task (val)
           (let ((task (pexec val)))
             (join task)
             task)))
    (is (= 1 (join (apply #'select-one (mapcar #'make-done-task '(1 2 3))))))))

(test select-one-error
  (signals simple-error (join (select-one (pexec (sleep 0.01) 1)
                                          (pexec (error "Error"))))))

