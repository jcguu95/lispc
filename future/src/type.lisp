(in-package :paren)

;; TODO Change syntax from (:array () :int) to (:array :int)

;; TODO Support `cast`. An example from https://cdecl.org/: cast foo into
;; block(int, long long) returning double
;;
;; (double (^)(int , long long ))foo

(defun type? (form)
  (cond ((keywordp form)
         form)
        ((listp form)
         (let ((kind (car form)))
           (cond
             ((find kind '(:pointer :c-pointer :v-pointer :cv-pointer))
              (let ((subtype (nth 1 form))
                    (pointer-count (nth 2 form)))
                (and (<= 2 (length form) 3)
                     (or (null pointer-count)
                         (and
                          (integerp pointer-count)
                          (> pointer-count 0)))
                     (type? subtype)
                     kind)))
             ((eq kind :function)
              (let ((to-type (nth 1 form))
                    (from-types (nth 2 form)))
                (and (= 3 (length form))
                     (type? to-type)
                     (every #'type? from-types)
                     kind)))
             ((eq kind :struct)
              (let ((name (nth 1 form)))
                (and (= 2 (length form))
                     (or (stringp name)
                         (keywordp name)) ; TODO better error message for failure
                     kind)))
             ((eq kind :type-of)
              (let ((name (nth 1 form)))
                (and (= 2 (length form))
                     (or (stringp name)
                         (symbolp name) ; TODO should we add this for :struct too?
                         (keywordp name)) ; TODO better error message for failure
                     kind)))
             ((eq kind :array)
              (let ((length (nth 1 form))
                    (subtype (nth 2 form)))
                (and (= 3 (length form))
                     ;; NOTE People may enter octal integers here in arrays..
                     ;; (or (null length)
                     ;;     (and (> length 0)
                     ;;          (integerp length)))
                     (type? subtype)
                     kind))))))
        (t (error "Unexpected form: ~a~%" form))))

(defun fmt-string<-type (type &optional no-filler)
  (assert (type? type))
  (let ((kind (type? type)))
    (cond
      ((find kind '(:pointer :c-pointer :v-pointer :cv-pointer))
       (assert (<= 2 (length type) 3))
       (let ((subtype (nth 1 type))
             (pointer-count (nth 2 type)))
         (unless pointer-count
           (setf pointer-count 1))
         (format nil
                 (fmt-string<-type subtype)
                 (format nil (if no-filler
                                 "~{~a~}"
                                 "~{~a~}(~~a)")
                         (make-list pointer-count :initial-element
                                    (case kind
                                      (:pointer "*")
                                      (:c-pointer "*const ")
                                      (:v-pointer "*volatile ")
                                      (:cv-pointer "*const volatile ")))))))
      ((eq kind :function)
       (assert (= (length type) 3))
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
      ((eq kind :struct)
       (assert (= 2 (length type)))
       (let ((name (nth 1 type)))
         (if no-filler
             (format nil "struct ~a" (invert-case (symbol-name name)))
             (format nil "struct ~a ~~a" (invert-case (symbol-name name))))))
      ((eq kind :type-of)
       (assert (= 2 (length type)))
       (let ((name (nth 1 type)))
         (if no-filler
             (format nil "typeof(~a)" (invert-case (symbol-name name)))
             (format nil "typeof(~a) ~~a" (invert-case (symbol-name name))))))
      ((eq kind :array)
       (assert (= 3 (length type)))
       (let ((length (nth 1 type))
             (subtype (nth 2 type)))
         (unless length (setf length ""))
         (format nil
                 (fmt-string<-type subtype)
                 (if no-filler
                     (format nil "[~a]" (c length))
                     (format nil "(~~a)[~a]" (c length))))))
      (t
       (format
        nil
        (if no-filler
            "~a"
            "~a ~~a")
        (cond
          ((keywordp kind)
           (case kind
             (:void "void")
             (:int "int") (:c-int "const int") (:v-int "volatile int") (:cv-int "const volatile int")
             (:short "short") (:c-short "const short") (:v-short "volatile short") (:cv-short "const volatile short")
             (:long "long") (:c-long "const long") (:v-long "volatile long") (:cv-long "const volatile long")
             (:long2 "long long") (:c-long2 "const long long") (:v-long2 "volatile long long") (:cv-long2 "const volatile long long")
             (:uint "unsigned int") (:c-uint "const unsigned int") (:v-uint "volatile unsigned int") (:cv-uint "const volatile unsigned int")
             (:ushort "unsigned short") (:c-ushort "const unsigned short") (:v-ushort "volatile unsigned short") (:cv-ushort "const volatile unsigned short")
             (:ulong "unsigned long") (:c-ulong "const unsigned long") (:v-ulong "volatile unsigned long") (:cv-ulong "const volatile unsigned long")
             (:ulong2 "unsigned long long") (:c-ulong2 "const unsigned long long") (:v-ulong2 "volatile unsigned long long") (:cv-ulong2 "const volatile unsigned long long")
             (:char "char") (:c-char "const char") (:v-char "volatile char") (:cv-char "const volatile char")
             (:schar "signed char") (:c-schar "const signed char") (:v-schar "volatile signed char") (:cv-schar "const volatile signed char")
             (:uchar "unsigned char") (:c-uchar "const unsigned char") (:v-uchar "volatile unsigned char") (:cv-uchar "const volatile unsigned char")
             (:float "float") (:c-float "const float") (:v-float "volatile float") (:cv-float "const volatile float")
             (:double "double") (:c-double "const double") (:v-double "volatile double") (:cv-double "const volatile double")
             (:ldouble "long double") (:c-ldouble "const long double") (:v-ldouble "volatile long double") (:cv-ldouble "const volatile long double")
             (t (resolve-symbol kind))))
          (t
           (error "Unknown kind: ~a.~%" kind))))))))
