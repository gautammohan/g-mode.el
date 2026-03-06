;;; -*- lexical-binding: t; -*-

;; Unit tests for gss.el

(require 'gss)
(require 'ert)
(require 'grt)

(ert-deftest gss-test-token-defs ()
  (grt-cleanup-symbols (gss--tokens)
    (setq gss--tokens nil)
    (should-error (gss-deftoken style) :type 'gss-bad-definition)
    (should (gss-deftoken color))
    (should (equal gss--tokens `((color . identity))))
    (let ((id (lambda (x) x)))
      (should (gss-deftoken bar id))
      (should-not (cl-set-exclusive-or gss--tokens `((color . identity) (bar . ,id)) :test #'equal)))))

(defmacro gss-test--with-tokens (toks &rest body)
  (declare (indent 1))
  `(grt-cleanup-symbols  (gss--tokens ,@toks)
     (eval-and-compile
       (setq gss--tokens nil)
       ,@(mapcar (lambda (tok) `(gss-deftoken ,tok)) toks))
     (debug)
     ,@body))

(ert-deftest gss-test-defpalette-parse ()
  ;; sanity checks
  (gss-test--with-tokens (foo bar)
    (grt-cleanup-symbols (p1)
      (should-error (gss-defpalette p1) :type 'gss-bad-parse)
      (should-error (gss-defpalette p1 :not-a-plist) :type 'gss-bad-parse)
      (should-error (gss-defpalette p1 :unknown-key 'val) :type 'gss-bad-parse))
    (grt-cleanup-symbols (p1)
      (should (macroexpand-1 (gss-defpalette p1 :foo 'val))))))


(defmacro gss--should-preserve-symbol-plist (sym &rest body)
  "Check the symbol-plist of sym is not altered after body"
  (let ((old-plist-var (make-symbol "old-plist"))
        (sym-var (make-symbol "sym")))
    `(let ((,sym-var ,sym)
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

(ert-deftest gss-test-palette-bad-calls ()
  (with-palette-sym p1
    ;; invalid calls to gss-defpalette should not alter the symbol as this would indicate a malformed palette initialization
    (gss--should-each-preserve-symbol-plist 'p1
      (should-error (gss-defpalette p1 :style) :type 'gss-bad-parse)
      (should-error (gss-defpalette p1) :type 'gss-bad-definition)
      (should-error (gss-defpalette p1 :foo 'bar) :type 'gss-bad-parse)
      (should-error (gss-defpalette p1 :style 'bar) :type 'gss-bad-parse)
      (should-error (gss-defpalette p1 :style '(ns . foo)) :type 'gss-bad-parse))))

(ert-deftest gss-test-palette-simple ()
  (with-palette-sym p1
    (with-style 'foo (lambda ())
      (gss-defpalette p1 :style (ns . foo))
      (should (get 'p1 'gss-palette))
      (should-error (gss-defpalette p1 :style (ns . foo)) :type 'gss-bad-definition)))
  (with-palette-sym p1
    (should (gss-defpalette p1 :style (ns . (lambda ()))))))

(ert-deftest gss-test-palette-multiple-styles ()
  (with-palette-sym p1
    (with-palette-sym p2
      (with-style 'foo (lambda ())
        (with-style 'bar (lambda ())
          (should (gss-defpalette p1 :style (ns1 . foo) :style (ns2 . bar)))
          (should (gss-defpalette p2 :style ((ns1 . foo) (ns2 . bar))))
          ;; Note: this should be an order-independent set equality but
          ;; I don't want to deal with writing that now.
          (should (equal (get 'p1 'gss-spec) (get 'p2 'gss-spec))))))))

(ert-deftest gss-test-palette-with-simple ()
  (with-palette-sym p1
    ;; (should-error (gss-with 'p1) :type 'gss-error)
    (gss-defpalette p1 :style (ns . (lambda ())))
    (should (gss-with 'p1))))

(ert-deftest gss-test-palette-def-tokens ()
  (with-palette-sym p1
    (with-token thing
      (gss-defpalette p1 :style (ns1 . (lambda ())))
      (let ((gss--current-palette 'p1))
        (gss-defthing name1 3)
        (symbol-plist 'p1)))))
