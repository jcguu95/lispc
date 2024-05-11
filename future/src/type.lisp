;; TODO Redo for the type system: Basic types are represented as keywords,
;; while composed types are represented as lists whose cars are of the
;; following: :pointer, :function, :struct, :array. The users can write
;; instead :pt, :fn, :{}, :[] respectively, but a normalizer will transform it
;; into the canonical, longer forms. Write util functions to handle and
;; inspect types. Allow users to define more type operators (e.g. :pt :fn as
;; above). Finally, write a printer, and integrate that with SET and DECLARE.
;; Mention that the type system is by no means complete, yet the user can
;; inline any C codes so that's not a problem.

(defun type? (form)
  (if (keywordp form)
      t ;; (find form *primitive-types*) ; TODO Implement *primitive-types*
      (let ((key (car form)))
        (case key
          (:pointer
           (let ((pointer-count (nth 1 form))
                 (subtype (nth 2 form)))
             (and (= 3 (length form))
                  (integerp pointer-count)
                  (> pointer-count 0)
                  (type? subtype))))
          (:function
           (let ((to-type (nth 1 form))
                 (from-types (nth 2 form)))
             (and (= 3 (length form))
                  (type? to-type)
                  (every #'type? from-types))))
          (:struct
           (let ((name (nth 1 form)))
             (and (= 2 (length form))
                  (or (stringp name)
                      (keywordp name)))))
          (:array
           (let ((length (nth 1 form))
                 (subtype (nth 2 form)))
             (and (= 3 (length form))
                  (or (null length)
                      (and (> length 0)
                           (integerp length)))
                  (type? subtype))))))))

(assert
 (and
  (type? '(:pointer 3 (:array nil (:struct :X))))
  (type? :integer)
  (type? '(:array 1 :integer))
  (type? '(:function :int (:int (:pointer 3 (:array nil :int)))))
  (not (type? '(:array 0 :integer)))
  (not (type? '(:pointer 0 :integer)))))
