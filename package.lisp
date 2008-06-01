(cl:defpackage :pcall
  (:use :cl :bordeaux-threads :pcall-queue)
  (:export #:pcall #:pexec #:plet #:join #:done-p
           #:*thread-pool* #:start-thread-pool #:stop-thread-pool
           #:make-thread-pool #:close-thread-pool #:pool-name))
