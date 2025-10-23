
;;; gss.el --- "Gmacs Style Sheets"

;; Author: Gautam Mohan <me@gautammohan.com>
;; Maintainers: Gautam Mohan <me@gautammohan.com>
;; Created: 2025-10-06
;; Version: 0.0.1
;; Keywords: emulations
;; URL: https://github.com/gautammohan/g-mode.el
;;; Commentary:

;; This file provides "GSS", a Sass/CSS-inspired system for specifying faces using style attributes. Style attributes are specified as .dotted.names which resolve to unique "base" style faces according to the namespace defined in gss-attributes. All GSS faces inherit from those base styles, so any alterations to them are inherited by all dependent faces.
;;
;; "Mixin" faces can also be defined to abstract sets of related style attributes and used to specify style attributes.
;;
;; TODO:
;; - implement (gss-attr ...) to create attributes and add them to the namespace
;; - figure out a customization interface/variant swapping
;; - figure out how to pass raw face attributes (for g-default)


;;; Code:

(require 'cl-lib)

(defgroup gss-faces nil
  "Gmacs Faces"
  :group 'g)

  ;; Default theme colors use Nord
(defconst gss-attributes
  (let (;; Polar Night: darkest to brightest
        (nord0 "#2E3440")
        (nord1 "#3B4252")
        (nord2 "#434C5E")
        (nord3 "#4C566A")
        ;; Snow Storm (w/custom white): darkest to brightest
        (nord4 "#D8DEE9")
        (nord5 "#E5E9F0")
        (nord6 "#ECEFF4")
        (nordw "#F8FAFC")
        ;; Frost: most-to-least contrasting
        (nord7 "#8FBCBB")
        (nord8 "#88C0D0")
        (nord9 "#81A1C1")
        (nord10 "#5E81AC")
        ;; Aurora: red, orange, yellow, green, purple
        (nord11 "#BF616A")
        (nord12 "#D08770")
        (nord13 "#EBCB8B")
        (nord14 "#A3BE8C")
        (nord15 "#B48EAD"))
    `((font . ((size . ,(defface gss--attr-font-size '((default . (:height 160))) "Gmacs internals -- DO NOT EDIT"))
               (monospace . ,(defface gss--attr-font-monospace '((default . (:family "Roboto Mono"))) "Gmacs internals -- DO NOT EDIT"))
               (proportional . ,(defface gss--attr-font-proportional '((default . (:family "Roboto"))) "Gmacs internals -- DO NOT EDIT"))
               (straight . ,(defface gss--attr-font-straight '((default . (:slant normal))) "Gmacs internals -- DO NOT EDIT"))
               (italic . ,(defface gss--attr-font-italic '((default . (:slant italic))) "Gmacs internals -- DO NOT EDIT"))
               (regular . ,(defface gss--attr-font-regular '((default . (:weight regular))) "Gmacs internals -- DO NOT EDIT"))
               (emph . ,(defface gss--attr-font-emph '((default . (:weight bold))) "Gmacs internals -- DO NOT EDIT"))
               (normal . ,(defface gss--attr-font-normal '((default . (:width regular))) "Gmacs internals -- DO NOT EDIT"))))
      (color . ((text . ((base . ,(defface gss--attr-text-base `((default . (:foreground ,nord0))) "Gmacs internals -- DO NOT EDIT"))
                         (secondary . ,(defface gss--attr-text-secondary `((default . (:foreground ,nord1))) "Gmacs internals -- DO NOT EDIT"))
                         (tertiary . ,(defface gss--attr-text-tertiary `((default . (:foreground ,nord2))) "Gmacs internals -- DO NOT EDIT"))
                         (quarternary . ,(defface gss--attr-text-quarternary `((default . (:foreground ,nord3))) "Gmacs internals -- DO NOT EDIT"))))
                (ui . ((background . ,(defface gss--attr-ui-background `((default . (:background ,nordw))) "Gmacs internals -- DO NOT EDIT"))
                       (secondary . ,(defface gss--attr-ui-secondary `((default . (:background ,nord6))) "Gmacs internals -- DO NOT EDIT"))
                       (tertiary . ,(defface gss--attr-ui-tertiary `((default . (:background ,nord5))) "Gmacs internals -- DO NOT EDIT"))
                       (quarternary . ,(defface gss--attr-ui-quarternary `((default . (:background ,nord4))) "Gmacs internals -- DO NOT EDIT"))))
                (accent . ((default . ,(defface gss--attr-accent-default `((default . (:foreground ,nord7))) "Gmacs internals -- DO NOT EDIT"))
                           (secondary . ,(defface gss--attr-accent-secondary `((default . (:foreground ,nord8))) "Gmacs internals -- DO NOT EDIT"))
                           (tertiary . ,(defface gss--attr-accent-tertiary `((default . (:foreground ,nord9))) "Gmacs internals -- DO NOT EDIT"))
                           (quarternary . ,(defface gss--attr-accent-quarternary `((default . (:foreground ,nord10))) "Gmacs internals -- DO NOT EDIT"))))
                (signal . ((error . ,(defface gss--attr-signal-error `((default . (:foreground ,nord11))) "Gmacs internals -- DO NOT EDIT"))
                           (issue . ,(defface gss--attr-signal-issue `((default . (:foreground ,nord12))) "Gmacs internals -- DO NOT EDIT"))
                           (warn . ,(defface gss--attr-signal-warn `((default . (:foreground ,nord13))) "Gmacs internals -- DO NOT EDIT"))
                           (ok . ,(defface gss--attr-signal-ok `((default . (:foreground ,nord14))) "Gmacs internals -- DO NOT EDIT"))
                           (alt . ,(defface gss--attr-signal-alt `((default . (:foreground ,nord15))) "Gmacs internals -- DO NOT EDIT"))))))
      (mod . ((extend . ,(defface gss--attr-mod-extend '((default . (:extend t))) "Gmacs internals -- DO NOT EDIT")))))))

(cl-defmacro gss-set (face &rest style)
  "Set a face to obey the style attrs specified. style arguments may
be attrs or mixins. Note the order of the attributes matters, with
earlier items taking precedence over later ones."
  ;; This macro interprets each of the dotted style accessors in the context of the gss-attributes let-alist and produces a quoted list of faces to pass to :inherit. Note that we need to use ":inherit (list ,@style)" instead of ":inherit ,style" because set-face-attribute would try to evaluate the expanded ,style form literally as a function, which we don't want. This same technique is used in gss-mixin to "defer" list construction to runtime.
  `(let-alist gss-attributes
     (face-spec-reset-face ,face)
     (set-face-attribute ,face nil :inherit (list ,@style))))

(defun gss-unset (face)
  "Restore previous attributes of face and make it no longer dependent
on GSS style attrs"
  (unless (eq face 'default)
    (face-spec-reset-face face)
    ;; Note: this is a nonstandard way to restore the original attrs of the face.
    ;; We are not using *-reset face functions since we are not modifying any of the property values
    ;; associated with existing emacs machinery for custom themes, faces, or specs.
    (apply #'set-face-attribute face nil (face-spec-choose (get face 'face-defface-spec)))))

(cl-defmacro gss-mixin (face docstr &rest style)
  "Define a \"mixin\" face using the provided style attributes. This
  must be a unique face name, preferably prefixed by \"gss-\". It can be passed to other gss-*
  functions as a style argument. Due to face inheritance semantics, a
  mixin should be at the end of the style attr list so it can be
  overridden by explicit styles."
  ;; This code constructs a defface expression with one face spec: "default". Each style argument is interpreted as "dotted access" of the gss-attributes nested alists. The macro is constructed carefully to ensure the "dotted accessors" are interpreted at runtime so they are meaningful in the context of (let-alist gss-attributes ...) while producing a form to match the face spec of ((SPEC . ATTRLIST)).
  ;; An example expansion:
  ;; (gss-mixin gss-highlight "Highlight background shade" .color.ui.secondary .mod.extend)
  ;; (defface gss-highlight `((default . (:inherit (,.color.ui.secondary
  ;;                                              ,.mod.extend)))) "Highlight background shade")
  `(let-alist gss-attributes
     (defface ,face (list  (cons 'default (list :inherit (list  ,@style)))) ,docstr :group 'g-faces)))

(defun gss--sync-default ()
  "This function syncs the \"default\" Emacs face to \"gss-default\", the
defined Gmacs default face which respects underlying gss attributes.
Whenever an attribute inherited by gss-default is updated, this
function must be called, since \"default\" is the face which ultimately
provides values for all unspecified attributes."
  (cl-loop for (attr . name) in face-attribute-name-alist do
           (set-face-attribute 'default nil attr (face-attribute 'gss-default attr nil t))
           ;; We need to redisplay between each call to set an attribute to avoid issues with batching
           ;; font spec (:height :slant :weight etc..) and font family changes. There are probably more
           ;; efficient ways to do it (first changing family, then the spec attrs) but this works for now
           (redisplay)))

;; The "gss-default" face is constructed by hand for now since it must be fully specified (so we can sync the default face attrs from it). Currently gss cannot pass "raw" face attributes via its definition functions, so we must do it this way.
(let-alist gss-attributes
  (defface gss-default `((default . (
                                   :foundry nil
                                   :underline nil
                                   :strike-through nil
                                   :box nil
                                   :inverse-video nil
                                   :stipple nil
                                   :extend nil
                                   :inherit (,.font.size
                                             ,.font.monospace
                                             ,.font.regular
                                             ,.font.straight
                                             ,.font.normal
                                             ,.color.text.base
                                             ,.color.ui.background)))) "Default props for frame text"))
(gss--sync-default)

(gss-mixin gss-highlight "Prominent Highlight" .color.ui.secondary .mod.extend)
(gss-set 'region .color.ui.secondary .mod.extend)

(provide 'gss)
