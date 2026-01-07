
;;; gss.el --- "Gmacs Style Sheets"

;; Author: Gautam Mohan <me@gautammohan.com>
;; Maintainers: Gautam Mohan <me@gautammohan.com>
;; Created: 2025-10-06
;; Version: 0.0.1
;; Keywords: emulations
;; URL: https://github.com/gautammohan/g-mode.el
;;; Commentary:

;; This file provides "GSS", a system to specify Emacs faces using
;; style attributes dependent on custom-defined palettes.

;; Each style attribute is built recursively from other style
;; attributes or "gss specs",which are atomic definitions of emacs
;; face-spec properties. GSS styles are stored in a "palette". Each
;; palette acts as an independent namespace to store specs and styles
;; and provides a context to the gss-set/defface/defstyle commands.

;; These commands accept concise .dotted.notation to traverse the
;; palette namespace and specify multiple specs and styles that a face
;; inherits from.

;;; Code:

(require 'cl-lib)

(defgroup gss-faces nil
  "Gmacs Faces"
  :group 'g)

;;; Palettes

(setq gss--palettes nil)
(cl-defmacro gss-defpalette (palette &optional (custom-prefix "gss") &rest specs)
  "Define a new palette where each plist entry :prefix func denotes
the namespace of a style.  A style can either be a lambda function
that takes a single token argument and (maybe) returns a facespec, or
a symbol referring to a \"registered style\" previously defined with gss-defstyle."
  ;; prefix frame name with gss-*, do a soft-intern check first for collisions.
  ;; check that "specs" is a proper plist.
  ;; check that all styles are valid. a symbol has to be in gss--styles
  ;; a lambda is passed as-is and assumed to do its own thing (check
  ;; for a certain type).
  `(progn 
     (put ',palette 'gss-spec ',specs)
     (push ',palette gss--palettes)))

;; should remain unbound globally and set implicitly within (gss-with ...) forms
(defvar gss--current-palette)

(defmacro gss-with (palette &rest body)
  `(let ((gss--current-palette ,palette))
     (let-alist gss--current-palette
       ,@body)))

;;; Tokens

(setq gss--tokens nil)
(cl-defmacro gss-deftoken (token &optional (override 'warn))
  "Define a GSS token and its constructor function \"gss-def<token>\""
  `(progn
     (if (memq ',token gss--tokens)
         (pcase ',override
           (t nil)
           ('warn (warn "Overriding existing GSS token \"%s\"" ,token))
           (_ (error "GSS token \"%s\" already defined" ,token)))
       (push ',token gss--tokens))
     (cl-defmacro ,(intern (concat "gss-def" (symbol-name token))) (name val)
       (gss--deftoken name ',token val))))

;; store tokens in an alist under 'gss-tokens prop of palette symbol
(defun gss--deftoken (name type val)
  (unless (get gss--current-palette (quote name))
    (setf (alist-get (quote name) (get gss--current-palette 'gss-tokens)) `(,type ,val))))

;;; Styles

(defvar gss--styles nil "GSS registered styles, can be referred to by symbol in palette definitions")
(defun gss-defstyle (key fun)
  "A style function takes one argument, a style token, and generates a
  an Emacs defface-compliant facespec from it. Once defined it is
  available to all palettes definitions by its key symbol. To limit a
  style function to a single palette, use a lambda in the palette's
  spec definition."
  (pcase (list key fun)
    ((guard (memq key gss--styles)) (error "GSS style \"%s\"
already registered" key))
    (`((pred symbolp) (pred functionp)) (setf (alist-get key
                                                         gss--styles)
                                              fun))
    (_ (error "Registered style functions are a lambda form taking a spec
type and spec val as arguments with a quoted name"))))

(defun gss--compute-styles (palette)
  ;; Pass all tokens defined in the palette to all its associated
  ;; style functions.

  ;; NOTE: this is currently "embarrassingly parallel" but if we add the ability
  ;; for tokens to depend on other tokens then the token loop must be ordered.
  (pcase-dolist (`(,name ,token) (get gss--current-palette 'gss-tokens))
    (pcase-dolist (`(,ns ,style) (get gss--current-palette 'gss-spec))
      ;; if the style function returns a non-nil spec value, we assign it to a unique face symbol 
      (if-let* ((spec (funcall style token))
                ;; NOTE: it's tempting to use an uninterned symbol here but I am not sure how that interacts
                ;; with the C-level implementation of face specs, which (I believe) assumes all face symbols
                ;; are interned for the entire runtime of Emacs.
                (sym (intern (string-join (mapcar #'symbol-name (list palette ns type name)) "-"))))
          (face-spec-set sym spec)))))

(defun gss-refresh (&rest palettes)
  ;; recompute the specified palettes or all palettes if no args passed
  ;; call gss--compute-styles on each one internally.
  )


;; Reify a color as .color.<fg,bg>, taking into account the theme variant.
(cl-defun gss--reify/color (spec context)
  (let* ((variant (alist-get 'variant context))
         (extract (cond
                   ((eq variant 'light) #'car)
                   ((eq variant 'dark) #'cdr)
                   (t (progn 
                        (display-warning 'gss
                                         (format "Unknown variant %s" variant)
                                         :warning)
                        #'car))))
         (result (cl-loop for (layer . name) in '((:foreground . fg) (:background . bg)) collect
                          `(,name .  ((((type graphic)) . (,layer ,(funcall extract (get spec 'gui))))
                                      (((type tty)) . (,layer ,(funcall extract (get spec 'tty)))))))))

    (map-into result 'hash-table)))


;; Reify a gss spec fragment into an actual Emacs face-spec. each
;; defined spec type must provide its own implementation function as
;; gss--reify/<spec>, similar to the use-package convention.

;; Reify a font attribute directly
(cl-defun gss--reify/attr (spec context)
  `((((type graphic)) . ,(get spec 'gui))
    (((type tty)) . ,(get spec 'tty))))

;;; Style Functions

(defmacro gss-set (face &rest styles)
  `(progn (face-spec-reset-face ,face)
          (set-face-attribute ,face nil :inherit (list ,@styles))))

(defmacro gss-defface (face doc &rest styles)
  `(progn (defface ,face nil ,doc)
          (gss-set ',face ,@styles)))

(defmacro gss-defalias (name &rest styles)
  ;; NOTE: we don't need to guard against recursive or mutually
  ;; recursive style aliases since name lookups are eager:
  ;;
  ;; (defstyle foo .foo) ;error: foo do does not exist
  ;; or
  ;; (defstyle foo .bar) ;error: bar does not exist
  ;; (defstyle bar .foo)
  ;;
  ;; Additionally, since styles cannot be redefined or removed, this case cannot occur:
  ;;
  ;; (defstyle foo .existing.thing)
  ;; (defstyle foo .foo) ;error: foo already defined.
  )

(provide 'gss)
