(in-package :paren)

(defun read-file-into-list (file-path)
  "Reads a string from a file and parses it into a Lisp list."
  (with-open-file (stream file-path :direction :input)
    (let ((file-contents (make-string (file-length stream))))
      (read-sequence file-contents stream)
      (read-from-string (format nil "(~a)" file-contents)))))

(defun replace-file-extension (file-path new-extension)
  "Replaces the file extension of FILE-PATH with NEW-EXTENSION."
  (let* ((pathname (parse-namestring file-path))
         (new-pathname (make-pathname :name (pathname-name pathname)
                                      :type new-extension
                                      :defaults pathname)))
    (namestring new-pathname)))

(defun compile-lsp-forms (lsp-forms &key (stream *standard-output*))
  (write-line
   (with-output-to-string (s)
     (loop :for form :in lsp-forms
           :for k :from 1
           :do (format s "~a" (c form))
           :do (when (< k (length lsp-forms))
                 (format s "~%~%"))))
   stream))

(defun c-path (lsp-file-path)
  "Return the path of the corresponding `.c` file."
  (replace-file-extension lsp-file-path "c"))

(defun compile-lsp-file (lsp-file-path)
  (with-open-file
      (stream (c-path lsp-file-path)
              :direction :output
              :if-exists :supersede)
    (let ((forms (read-file-into-list lsp-file-path)))
      (compile-lsp-forms forms :stream stream))))

(defun read-file-to-string (filename)
  "Reads the entire contents of a file into a string."
  (with-open-file (stream filename :direction :input :external-format :utf-8)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

;; (progn
;;   (compile-lsp-file "../examples/hello-world.lsp")
;;   (compile-lsp-file "../examples/switch.lsp")
;;   (compile-lsp-file "../examples/cond.lsp")
;;   (compile-lsp-file "../examples/control-flow.lsp")
;;   (compile-lsp-file "../examples/macro-example.lsp")
;;   (compile-lsp-file "../examples/type-struct-example.lsp")
;;   (compile-lsp-file "../examples/higher-order-function.lsp")
;;   (compile-lsp-file "../examples/c-macro.lsp")
;;   (compile-lsp-file "../examples/nested-loops.lsp")
;;   (compile-lsp-file "../examples/macro-example.lsp")
;;   ;; (compile-lsp-file "../examples/sectorlisp.lsp")
;;   )
