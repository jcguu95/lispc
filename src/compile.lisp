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

(defun compile-parenc-forms (parenc-forms &key (stream *standard-output*))
  (write-line
   (with-output-to-string (s)
     (loop :for form :in parenc-forms
           :for k :from 1
           :do (format s "~a" (c form))
           :do (when (< k (length parenc-forms))
                 (format s "~%~%"))))
   stream))

(defun c-path (parenc-file-path)
  "Return the path of the corresponding `.c` file."
  (replace-file-extension parenc-file-path "c"))

(defun compile-parenc-file (parenc-file-path)
  (with-open-file
      (stream (c-path parenc-file-path)
              :direction :output
              :if-exists :supersede)
    (let ((forms (read-file-into-list parenc-file-path)))
      (compile-parenc-forms forms :stream stream))))

(defun read-file-to-string (filename)
  "Reads the entire contents of a file into a string."
  (with-open-file (stream filename :direction :input :external-format :utf-8)
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

;; (progn
;;   (compile-parenc-file "../examples/hello-world.parenc")
;;   (compile-parenc-file "../examples/switch.parenc")
;;   (compile-parenc-file "../examples/cond.parenc")
;;   (compile-parenc-file "../examples/control-flow.parenc")
;;   (compile-parenc-file "../examples/macro-example.parenc")
;;   (compile-parenc-file "../examples/type-struct-example.parenc")
;;   (compile-parenc-file "../examples/higher-order-function.parenc")
;;   (compile-parenc-file "../examples/c-macro.parenc")
;;   (compile-parenc-file "../examples/nested-loops.parenc")
;;   (compile-parenc-file "../examples/macro-example.parenc")
;;   (compile-parenc-file "../examples/sectorlisp.parenc")
;;   )
