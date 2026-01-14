;;; -*- lexical-binding: t; -*-

(require 'gss)

(describe "defpalette: "
  (it "rejects & cleans up invalid specs"
    (expect (gss-defpalette 'p1 :style) :to-throw 'gss-bad-parse)))
