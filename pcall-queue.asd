(asdf:defsystem :pcall-queue
  :depends-on (:bordeaux-threads)
  :components ((:file "queue")))
