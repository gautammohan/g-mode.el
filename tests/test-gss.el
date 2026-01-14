;;; -*- lexical-binding: t; -*-
;; Tests for gss.el. Current convention for the test suite is to do all cleanup in the (before-each ...) form and use (after-each ...) for post-conditions that should hold true for the entire suite.
(require 'gss)

(setq gss-test-counter 0)
(cl-defun fresh (&optional (prefix "test"))
  "Generate a fresh symbol with an optionally-specified prefix."
  (intern (format (string-join (list prefix (number-to-string (cl-incf gss-test-counter))) ":"))))

;; Note: I don't love that this test has 't1' hardcoded but I don't see a better way currently because (deftoken ...) takes an unevaluated name as an argument which thwarts us from using variables holding symbol names since the variable name would be used to create the new definition instead of the symbol itself.
(describe "Tokens"
  (before-all
    (setq gss--tokens nil))
  (after-all
    (setq gss--tokens nil)
    (fmakunbound 'gss-deft1))
  (it "new token produces a properly-named macro def"
    (gss-deftoken t1)
    (let ((fname (concat "gss-def" "t1")))
      (print fname)
      (expect (intern-soft fname))
      (expect (macrop (intern-soft fname)))
      (expect gss--tokens :to-contain 't1)))
  (it "cannot redefine tokens"
    (expect (gss-deftoken t1) :to-throw 'gss-bad-definition))
  (it "cannot clobber existing gss-def* names"
    (expect (gss-deftoken alias) :to-throw 'gss-bad-definition)))

(describe "Styles"
  (after-all
    (setq gss--styles nil))
  (it "can define a style"
    (let ((fun (lambda ())))
      (expect (gss-defstyle 'foo fun) :not :to-throw)
      (expect gss--styles :to-contain (cons 'foo fun))))
  (it "cannot redefine a style"
    (expect (gss-defstyle 'foo (lambda ())) :to-throw 'gss-bad-definition)))

(describe "Palettes"
  :var (p-defined (fresh "palette"))
  (describe "defpalette invalid calls"
    :var ((p1 (fresh "palette")))
    ;; Postcondition:
    ;; defpalette calls that throw errors should not save any gss symprops
    ;; Note: since we use fresh symbols all their symprops are nil by default
    (after-each
      (expect (symbol-plist p1) :to-be nil))
    (it "rejects empty :style arg"
      (expect (gss-defpalette p1 :style) :to-throw 'gss-bad-parse))
    (it "empty palette spec does nothing"
      (expect  (gss-defpalette p1) :to-throw 'gss-bad-definition))
    (it "errors on unknown keywords"
      (expect (gss-defpalette :foo 'bar) :to-throw 'gss-bad-parse)))
  (describe "defpalette valid calls"
    :var ((p1 (fresh "palette"))
          (p2 (fresh "palette")))
    (before-all
      (gss-deftoken foo))))
