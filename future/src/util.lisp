(in-package :paren)

;; for debugging
(defun inspect! (&rest rest) (break) rest)

(defun invert-case (string)
  "If all English characters in the string are uppercase, convert them to lowercase.
If all are lowercase, convert them to uppercase. If the characters are mixed
case, leave the string unchanged."
  (if (string= "" string)
      string
      (let ((has-upper nil)
            (has-lower nil))
        ;; Classify the string
        (dolist (char (coerce string 'list))
          (cond
            ((upper-case-p char) (setf has-upper t))
            ((lower-case-p char) (setf has-lower t))))
        (cond
          ((and has-upper (not has-lower))
           (map 'string #'char-downcase string))
          ((and has-lower (not has-upper))
           (map 'string #'char-upcase string))
          (t
           string)))))

;; TODO When writing the documentation for PAREN, show the reader how to use
;; #'indent to indent the generated code properly.
(defun indent (input-string &key (space-count 2))
  "Modify INPUT-STRING so that each #\Newline is followed by two spaces,
   but handle sequences of #\Newline such that spaces are added only after the last one.
   Also, add two spaces at the beginning if the first character isn't a #\Newline.
   Do not add spaces if the last #\Newline is the final character."
  (let ((result "")
        (in-newline-sequence nil)
        (first-char-processed nil)
        (last-char-newline nil)
        (indentation (format nil "~{~a~}" (make-list space-count :initial-element " "))))
    (loop for char across input-string do
      (cond
        ;; If the first character isn't a #\Newline, prepend two spaces
        ((and (not first-char-processed)
              (not (char= char #\Newline)))
         (setq result (concatenate 'string result indentation (string char)))
         (setq first-char-processed t))
        ;; If the first character is a #\Newline
        ((not first-char-processed)
         (setq result (concatenate 'string result (string char)))
         (setq first-char-processed t)
         (setq in-newline-sequence t))
        ;; If we encounter a #\Newline
        ((char= char #\Newline)
         (setq result (concatenate 'string result (string char)))
         (setq in-newline-sequence t)
         (setq last-char-newline t))
        ;; If we encounter any other character after a #\Newline sequence
        (in-newline-sequence
         (setq result (concatenate 'string result indentation (string char)))
         (setq in-newline-sequence nil)
         (setq last-char-newline nil))
        ;; Default case: just add the character
        (t
         (setq result (concatenate 'string result (string char)))
         (setq last-char-newline nil))))
    ;; If the string ends with a newline sequence, do not add spaces
    (when (and in-newline-sequence (not last-char-newline))
      (setq result (concatenate 'string result indentation)))
    result))

(defun resolve-symbol (symbol)
  (invert-case (substitute #\_ #\- (symbol-name symbol))))

(defun resolve-declaration (declaration)
  (assert (= 2 (length declaration)))
  (let* ((variable (resolve-symbol (nth 0 declaration)))
         (type (nth 1 declaration)))
    (format nil (fmt-string<-type type) variable)))
