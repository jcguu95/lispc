(in-package :paren)

;; TODO Change syntax from (:array () :int) to (:array :int)

;; TODO Support `volatile pointer`. (an example from https://cdecl.org/):
;; declare bar as volatile pointer to array 64 of const int
;; (and other register-related keywords)?
;;
;; const int (* volatile bar)[64]

;; TODO Support `cast`. An example from https://cdecl.org/: cast foo into
;; block(int, long long) returning double
;;
;; (double (^)(int , long long ))foo

(defun type? (form)
  (if (keywordp form)
      form
      (let ((kind (car form)))
        (case kind
          (:pointer
           (let ((subtype (nth 1 form))
                 (pointer-count (nth 2 form)))
             (and (<= 2 (length form) 3)
                  (or (null pointer-count)
                      (and
                       (integerp pointer-count)
                       (> pointer-count 0)))
                  (type? subtype)
                  kind)))
          (:function
           (let ((to-type (nth 1 form))
                 (from-types (nth 2 form)))
             (and (= 3 (length form))
                  (type? to-type)
                  (every #'type? from-types)
                  kind)))
          (:struct
           (let ((name (nth 1 form)))
             (and (= 2 (length form))
                  (or (stringp name)
                      (keywordp name)) ; TODO better error message for failure
                  kind)))
          (:array
           (let ((length (nth 1 form))
                 (subtype (nth 2 form)))
             (and (= 3 (length form))
                  (or (null length)
                      (and (> length 0)
                           (integerp length)))
                  (type? subtype)
                  kind)))))))

(defun fmt-string<-type (type &optional no-filler)
  (assert (type? type))
  (let ((kind (type? type)))
    (case kind
      (:pointer
       (let ((subtype (nth 1 type))
             (pointer-count (nth 2 type)))
         (unless pointer-count
           (setf pointer-count 1))
         (format nil
                 (fmt-string<-type subtype)
                 (if no-filler
                     (format nil "~{~a~}"
                             (make-list pointer-count :initial-element '*))
                     (format nil "~{~a~}(~~a)"
                             (make-list pointer-count :initial-element '*))))))
      (:function
          (let ((to-type (nth 1 type))
                (from-types (nth 2 type)))
            (format nil
                    (fmt-string<-type to-type)
                    (if no-filler
                        (format nil "(~{~a,~})"
                                (loop :for type :in from-types
                                      :collect (fmt-string<-type type t)))
                        ;; NOTE We must add extra parenthesis for function
                        ;; type. An opaque example is
                        ;;
                        ;;   char *((*((*(*(((foo)[])[8]))) ()))[])
                        ;;
                        ;; which means
                        ;;
                        ;;   declare foo as array of array 8 of pointer to
                        ;;   pointer to function returning pointer to array of
                        ;;   pointer to char
                        ;;
                        ;; But if you remove the parenthesis for the function
                        ;; part, you get
                        ;;
                        ;;   char *((*(*(*(((foo)[])[8])) ()))[])
                        ;;
                        ;; which means
                        ;;
                        ;;   declare foo as array of array 8 of pointer to
                        ;;   function returning pointer to pointer to array of
                        ;;   pointer to char
                        ;;
                        ;; according to https://cdecl.org/
                        ;;
                        ;; TODO I should come up with an simpler example to
                        ;; illustrate the point.
                        (format nil "(~~a) (~{~a~^,~})"
                                (loop :for type :in from-types
                                      :collect (fmt-string<-type type t)))))))
      (:struct
       (let ((name (nth 1 type)))
         (if no-filler
             (format nil "struct ~a" (invert-case (symbol-name name)))
             (format nil "struct ~a ~~a" (invert-case (symbol-name name))))))
      (:array
       (let ((length (nth 1 type))
             (subtype (nth 2 type)))
         (unless length (setf length ""))
         (format nil
                 (fmt-string<-type subtype)
                 (if no-filler
                     (format nil "[~a]" length)
                     (format nil "(~~a)[~a]" length)))))
      (t
       (format nil
               (if no-filler
                   "~a"
                   "~a ~~a")
               (invert-case (symbol-name kind)))))
    ))
