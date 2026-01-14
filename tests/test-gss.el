;;; -*- lexical-binding: t; -*-

(require 'gss)

(setq gss-test-counter 0)
(cl-defun fresh-sym (&optional (prefix "test"))
  (intern (format (string-join (list prefix (number-to-string (cl-incf gss-test-counter))) ":"))))

(describe "defpalette: "
  :var ((p1 (fresh-sym)))
  (after-each
    (cl-remprop p1 'gss-spec)
    (cl-remprop p1 'gss-palette))
  (it "rejects & cleans up bad :style args"
    (expect (gss-defpalette p1 :style) :to-throw 'gss-bad-parse)
    (expect (get p1 'gss-spec) :to-be nil))
  (it "empty palette spec does nothing"
    (gss-defpalette p1)
    (expect (get p1 'gss-spec) :to-be nil)
    (expect (get p1 'gss-palette) :to-be nil)))
