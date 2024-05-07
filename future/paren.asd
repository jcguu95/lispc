(cl:in-package #:asdf-user)

(defsystem #:paren
  :description "PAREN: A lispy langauge compiling to C."
  :author ("Jin-Cheng Guu <jcguu95@gmail.com>")
  :maintainer "Jin-Cheng Guu <jcguu95@gmail.com>"
  :homepage "https://github.com/jcguu95/lispc"
  :version "0.0.0"
  :license "MIT"
  :bug-tracker "https://github.com/jcguu95/lispc"
  :source-control (:git "https://github.com/jcguu95/lispc.git")
  :depends-on (:log4cl)
  :perform
  (test-op
   (o system)
   (declare (ignore o system))
   (asdf:load-system :paren.test))
  :components
  ((:module "src"
    :serial t
    :components ((:file "packages")
                 (:file "paren")))))

(defsystem #:paren.test
  :description "Tests for PAREN."
  :author ("Jin-Cheng Guu <jcguu95@gmail.com>")
  :maintainer "Jin-Cheng Guu <jcguu95@gmail.com>"
  :homepage "https://github.com/jcguu95/lispc"
  :version "0.0.0"
  :license "MIT"
  :bug-tracker "https://github.com/jcguu95/lispc"
  :source-control (:git "https://github.com/jcguu95/lispc.git")
  :depends-on (:paren :fiveam)
  :components
  ((:module "test"
    :serial t
    :components ((:file "packages")
                 (:file "paren.test"))))
  :perform
  (load-op                              ; Test whenever load.
   (o system)
   (declare (ignore o))
   (format t "Testing system ~a..~%" system)
   (uiop:symbol-call :fiveam '#:run! (find-symbol "PAREN.TEST" "PAREN.TEST"))))
