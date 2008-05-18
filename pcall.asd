(asdf:defsystem :pcall
  :depends-on (:bordeaux-threads :pcall-queue)
  :components ((:file "package")
               (:file "task" :depends-on ("package"))
               (:file "pool" :depends-on ("task"))
               (:file "pcall" :depends-on ("pool"))))

(asdf:defsystem :pcall-tests
  :depends-on (:pcall :fiveam)
  :components ((:file "tests")))