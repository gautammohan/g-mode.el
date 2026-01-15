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
  (declare (indent 2))
  `(let ((gss--styles gss--styles))
     (progn (gss-defstyle ,style ,fun)
            ,@body)))

(describe "Palettes"
  :var (p-defined (fresh "palette"))
  (describe "defpalette invalid calls"
    :var ((p1 (fresh "palette")))
    ;; Postcondition:
    ;; defpalette calls that throw errors should not save any gss symprops.
    ;; Since we use fresh symbols all their symprops are nil by default.
    (after-each
      (expect (symbol-plist p1) :to-be nil))
    (it "rejects empty :style arg"
      (expect (gss-defpalette p1 :style) :to-throw 'gss-bad-parse))
    (it "empty palette spec does nothing"
      (expect  (gss-defpalette p1) :to-throw 'gss-bad-definition))
    (it "errors on unknown keywords"
      (expect (gss-defpalette :foo 'bar) :to-throw 'gss-bad-parse))))

(defmacro with-palette-var (var &rest body)
  (declare (indent 1))
  `(let ((,var ',var))
     (unwind-protect
         (progn ,@body)
       (cl-remprop ,var 'gss-spec)
       (cl-remprop ,var 'gss-palette))))

(ert-deftest gss-test-simple-palette ()
  (require 'gss)
  (with-palette-var p1
    (with-style 'foo (lambda ())
      (gss-defpalette p1 :style '(ns . foo))
      (should (get 'p1 'gss-palette)))))

(with-palette-var p1
  (with-style 'foo (lambda ())
    (with-style 'bar (lambda ())
      (gss-defpalette p1 :style '(ns . foo)))))

