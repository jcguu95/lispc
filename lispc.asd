(cl:in-package #:asdf-user)

(defsystem #:lispc
  :description "LISP/c defines a lispy langauge, and compiles it into C."
  :author ("Jonathan Baca <https://github.com/eratosthenesia>"
           "Jin-Cheng Guu <jcguu95@gmail.com>")
  :maintainer "Jin-Cheng Guu <jcguu95@gmail.com>"
  :homepage "https://github.com/jcguu95/lispc"
  :version "1.1.0"
  :license "MIT"
  :bug-tracker "https://github.com/jcguu95/lispc"
  :source-control (:git "https://github.com/jcguu95/lispc.git")
  :depends-on ()
  :components
  ((:module #:src
    :serial t
    :components ((:file "packages")
                 (:file "utils"   )
                 (:file "c"       )
                 (:file "compile" )))))
