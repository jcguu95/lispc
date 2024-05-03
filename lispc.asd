(cl:in-package #:asdf-user)

(defsystem #:lispc
  :description "LISP/c: A lispy langauge compiling to C."
  :author ("Jonathan Baca <https://github.com/eratosthenesia>"
           "Jin-Cheng Guu <jcguu95@gmail.com>")
  :maintainer "Jin-Cheng Guu <jcguu95@gmail.com>"
  :homepage "https://github.com/jcguu95/lispc"
  :version "1.1.0"
  :license "MIT"
  :bug-tracker "https://github.com/jcguu95/lispc"
  :source-control (:git "https://github.com/jcguu95/lispc.git")
  :depends-on (:log4cl)

  :components
  ((:module "src"
    :serial t
    :components ((:file "packages")
                 (:file "utils")
                 (:file "c")
                 (:file "compile")))
   (:module "test"
    :components ((:static-file "test.c")  (:static-file "test.cl")
                 (:static-file "cuda.c")  (:static-file "cuda.cl")
                 (:static-file "myls.c")  (:static-file "myls.cl")
                 (:static-file "multi.c") (:static-file "multi.cl")))
   (:static-file "README.md")
   (:static-file "README.old.md")
   (:static-file "LICENSE.txt"))

  :perform
  (load-op                              ; Test whenever load.
   (o system)
   "DOC: Compile test *.cl files, and compare the results with the precompiled ones."
   (declare (ignore o))
   (format t "~%Testing system: ~a..~%" system)
   (loop :for name :in '("test" "cuda" "myls" "multi")
         :do (let ((*package* (find-package :lispc)) ; FIXME Why is this necessary? Something is wrong in c.lisp.
                   (tmp-file (uiop:merge-pathnames*
                              (format nil "~a-~a" (get-universal-time) (random 100000))
                              (uiop:temporary-directory))))
               ;; Compile NAME.cl to TMP-FILE with LISPC.
               (uiop:symbol-call
                :lispc '#:c-file<-cl-file
                (asdf:system-relative-pathname system (format nil "test/~a.cl" name))
                tmp-file)
               ;; Compare the result with the pre-compiled result under ./test.
               (unless
                   (string= (uiop:read-file-string tmp-file)
                            (uiop:read-file-string (asdf:system-relative-pathname
                                                    system (format nil "test/~a.c" name))))
                 (format t "Test failed for <~a>; signaling an error..~%" name)
                 (error "Wrong."))
               ;; FIXME It should not be redefining functions while compiling.. e.g.
               ;; > WARNING: redefining LISPC::LOOP-N-C in DEFUN
               ;; > WARNING: redefining LISPC::VOIDINT-C in DEFUN
               ;; > WARNING: redefining LISPC::INTVOID-C in DEFUN
               ;; > Test passes for multi.
               (format t "Test passed for <~a>.~%" name)
               (uiop:delete-file-if-exists tmp-file)))))
