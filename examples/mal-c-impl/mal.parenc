(include :system ("stdlib.h" "stdio.h" "string.h" "unistd.h")
         :local ("types.h" "readline.h" "reader.h" "core.h" "interop.h"))

(declare () (|eval|
             (:function (:pointer |MalVal|)
                        ((ast (:pointer |MalVal|))
                         (env (:pointer |Env|))))))

(declare () (|quasiquote|
             (:function (:pointer |MalVal|)
                        ((ast (:pointer |MalVal|))))))

;; // read
;; MalVal *READ(char prompt[], char *str) {
;;     char *line;
;;     MalVal *ast;
;;     if (str) {
;;         line = str;
;;     } else {
;;         line = _readline(prompt);
;;         if (!line) {
;;             _error("EOF");
;;             return NULL;
;;         }
;;     }
;;     ast = read_str(line);
;;     if (!str) { MAL_GC_FREE(line); }
;;     return ast;
;; }
(defun |read| (:pointer |MalVal|) ((prompt (:array :char))
                                   (str (:pointer :char)))
  (declare () (line (:pointer :char)))
  (declare () (ast (:pointer :|MalVal|)))
  (cond (str
         (set line str))
        (t
         (set line (@-readline prompt))
         (cond ((not line)
                (@-error (str "EOF"))
                (return "NULL")))))
  (set ast (@read-str line))
  (cond ((not str)
         (|@mal-gc-free| line)))
  (return ast))

;; // eval
;; int starts_with(MalVal *ast, const char *sym) {
;;     if (ast->type != MAL_LIST)
;;         return 0;
;;     const MalVal * const a0 = _first(ast);
;;     return (a0->type & MAL_SYMBOL) && ! strcmp(sym, a0->val.string);
;; }
(defun starts-with :int ((ast (:pointer |MalVal|))
                         (sym (:constant (:pointer :char))))
  (cond ((not (== (-> ast type)
                  |mal-list|))
         (return 0)))
  (declare () (a0 (:constant (:pointer (:constant :|MalVal|))))
           (@-first ast))
  (return (&& (& (-> a0 type)
                 |mal-symbol|)
              (not (@strcmp sym
                            (-> a0 (. val string)))))))

;; MalVal *qq_iter(GArray *xs) {
;;     MalVal *acc = _listX(0);
;;     int i;
;;     for (i=xs->len-1; 0<=i; i--) {
;;         MalVal * const elt = g_array_index(xs, MalVal*, i);
;;         if (starts_with(elt, "splice-unquote"))
;;             acc = _listX(3, malval_new_symbol("concat"), _nth(elt, 1), acc);
;;         else
;;             acc = _listX(3, malval_new_symbol("cons"), quasiquote(elt), acc);
;;     }
;;     return acc;
;; }
(defun qq-iter (:pointer :|MalVal|) ((xs (:pointer :|GArray|)))
  (declare () (acc (:pointer :|MalVal|))
           (@-|listX| 0))
  (declare () (i :int))
  (for ((set i (- (-> xs len) 1))
        (<= 0 i)
        (-- i))
       (declare () (elt (:const (:pointer :|MalVal|)))
                (@g-array-index xs "MalVal*" i)) ; NOTE What is this MalVal*?
       (cond ((@starts-with elt (str "splice-unquote"))
              (set acc (|@-listX| 3
                                  (@malval-new-symbol (str "concat"))
                                  (@-nth elt 1)
                                  acc)))
             (t
              (set acc (|@-listX| 3
                                  (@malval-new-symbol (str "cons"))
                                  (@quasiquote elt )
                                  acc)))))
  (return acc))

;; MalVal *quasiquote(MalVal *ast) {
;;     switch (ast->type) {
;;     case MAL_LIST:
;;         if (starts_with(ast, "unquote"))
;;             return _nth(ast, 1);
;;         else
;;             return qq_iter(ast->val.array);
;;     case MAL_VECTOR:
;;         return _listX(2, malval_new_symbol("vec"), qq_iter(ast->val.array));
;;     case MAL_HASH_MAP:
;;     case MAL_SYMBOL:
;;         return _listX(2, malval_new_symbol("quote"), ast);
;;     default:
;;         return ast;
;;     }
;; }
(defun quasiquote (:pointer :|MalVal|)
  ((ast (:pointer :|MalVal|)))
  (case (-> ast type)
    (|mal-list|
     (cond ((@starts-with ast (str "unquote"))
            (return (@-nth ast 1)))
           (t
            (return (@qq-iter (-> ast (. val array)))))))
    (|mal-vector|
     (return (|@-listX| 2
                        (@malval-new-symbol (str "vec"))
                        (@qq-iter (-> ast (. val array))))))
    (|mal-hash-map|)
    (|mal-symbol|
     (return (|@-listX| 2
                        (@malcal-new-symbol (str "quote"))
                        ast)))
    (t (return ast))))
