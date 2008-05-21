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
  (loop :until (zerop (queue-length q)) :do (sleep .01))
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

(defmacro in-thread-pool (&body body)
  `(let ((*thread-pool* (make-thread-pool 5)))
     (unwind-protect (progn ,@body)
       (close-thread-pool *thread-pool*))))

(test pcall-sanity
  (in-thread-pool
    (is (equal '(1 2 3) (join (prun (list 1 2 3)))))
    (let ((task (prun (+ 4 2))))
      (sleep .01)
      (is (= 6 (join task))))))

(test pcall-stress
  (flet ((compute ()
           (loop :for i :from 0 :below 100000
                 :sum (* i i))))
    (in-thread-pool
      (let ((tasks (loop :for i :from 0 :below 1000 :collect (pcall #'compute)))
            (answer (compute)))
        (sleep .05)
        (is (every (lambda (tsk) (= (join tsk) answer)) tasks))))))

