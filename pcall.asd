(asdf:defsystem :pcall
  :depends-on (:bordeaux-threads :pcall-queue)
  :components ((:file "package")
               (:file "task" :depends-on ("package"))
               (:file "pool" :depends-on ("task"))
               (:file "pcall" :depends-on ("task" "pool"))))

(asdf:defsystem :pcall-tests
  :depends-on (:pcall :fiveam)
  :components ((:file "tests")))

(defmethod asdf:perform ((op asdf:test-op) (system (eql (find-system :pcall))))
  (asdf:oos 'asdf:load-op :pcall)
  (asdf:oos 'asdf:load-op :pcall-tests)
  (funcall (intern (string :run!) (string :it.bese.FiveAM)) :pcall))
