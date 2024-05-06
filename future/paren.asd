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
  :perform
  (load-op                              ; Test whenever load.
   (o system)
   (declare (ignore o))
   (log:info "Testing system ~a." system)
   (fiveam:run! 'paren.test::paren.test))
  :components
  ((:module "test"
    :serial t
    :components ((:file "packages")
                 (:file "paren.test")))))
