;;; -*- lexical-binding: t; -*-

;; Unit tests for gss.el

(require 'gss)

(setq gss-test-counter 0)

(ert-deftest gss-test-token-defs ()
  ;; Note: I don't love this test having the defined function symbol name hardcoded but I don't see a better way to do it currently.
  (unwind-protect
      (let ((gss--tokens nil))
        (gss-deftoken t1)
        (should (intern-soft "gss-deft1"))
        (should (macrop (intern "gss-deft1")))
        (should-error (gss-deftoken style) :type 'gss-bad-definition)
        (should-error (gss-deftoken t1) :type 'gss-bad-definition)))
  (fmakunbound (intern "gss-deft1")))

(defmacro with-token (tok &rest body)
  "Define a token type that exists only within this block"
  (declare (indent 1))
  `(let ((gss--tokens gss--tokens))
     (unwind-protect
         (progn
           (gss-deftoken ,tok)
           ,@body)
       (fmakunbound (intern (concat "gss-def" (symbol-name ',tok)))))))

(ert-deftest gss-test-style-defs ()
  (let ((gss--styles nil)
        (fun (lambda ())))
    (should (gss-defstyle 'foo fun))
    (should (equal (assq 'foo gss--styles) `(foo . ,fun)))
    (should-error (gss-defstyle 'foo fun) :type 'gss-bad-definition)
    (should-error (gss-defstyle 'bar 'zap) :type 'gss-bad-parse)))

(defmacro with-style (style fun &rest body)
  "Define a style and associated stylefun that exist only within this block."
  (declare (indent 2))
  ;; cleanup works because gss--styles is dynamically scoped
  `(let ((gss--styles gss--styles))
     (progn (gss-defstyle ,style ,fun)
            ,@body)))

(defmacro with-palette-sym (sym &rest body)
  "Specify a symbol to be treated as a palette within this context"
  (declare (indent 1))
  (let ((sym-var (make-symbol "sym")))
    `(let ((,sym-var ',sym))
       (unwind-protect
           (progn ,@body)
         (cl-remprop ,sym-var 'gss-spec)
         (cl-remprop ,sym-var 'gss-palette)))))

(defmacro gss--should-preserve-symbol-plist (sym &rest body)
  "Check the symbol-plist of sym is not altered after body"
  (let* ((old-plist-var (make-symbol "old-plist"))
         (sym-var (make-symbol "sym")))
    `(let* ((,sym-var ,sym)
            (,old-plist-var (copy-sequence (symbol-plist ,sym))))
       (progn ,@body
              (should (equal ,old-plist-var (symbol-plist ,sym)))))))

(defmacro gss--should-each-preserve-symbol-plist (sym &rest forms)
  (declare (indent 1))
  ;; Note: This is a specialized version of a more generic
  ;; wrapper-foreach macro pattern that one might make in a proper
  ;; combinator library, simply to avoid currying the symbol arg.
  ;; 
  ;; Such a combinator might look like
  ;;  (gss--foreach-wrapper (wrapper args &rest forms)
  ;;   `(progn ,@(mapcar (lambda (form) `(,wrapper ,@args ,form)) forms)))
  ;; 
  ;; but I am not confident enough in it to use it.
  `(progn ,@(mapcar (lambda (form) `(gss--should-preserve-symbol-plist ,sym ,form)) forms)))

(ert-deftest gss-test-palette-defs ()
  (with-palette-sym p1
    ;; invalid calls to gss-defpalette should not alter the symbol as this would indicate a malformed palette initialization
    (gss--should-each-preserve-symbol-plist 'p1
      (should-error (gss-defpalette p1 :style) :type 'gss-bad-parse)
      (should-error (gss-defpalette p1) :type 'gss-bad-definition)
      (should-error (gss-defpalette p1 :foo 'bar) :type 'gss-bad-parse))))

(ert-deftest gss-test-palette-simple ()
  (with-palette-sym p1
    (with-style 'foo (lambda ())
      (gss-defpalette p1 :style '(ns . foo))
      (should (get 'p1 'gss-palette)))))

(with-palette-sym p1
  (with-style 'foo (lambda ())
    (with-style 'bar (lambda ())
      (gss-defpalette p1 :style '(ns . foo)))))
