
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

(define-error 'gss-error "GSS Error")
(define-error 'gss-bad-definition "GSS bad definition" 'gss-error)
(define-error 'gss-bad-parse "GSS improper form" 'gss-error)

(defun gss--symcat (&rest symbols)
  (intern (string-join (mapcar #'symbol-name symbols) "-")))

(cl-defmacro gss-defpalette (palette &rest specs)
  `(gss--defpalette ',palette (list ,@specs)))

(cl-defun gss--defpalette (palette specs)
  "Define a new palette with associated style specification.
Each style spec (specified using :spec) is a cons cell (NS . STYLE)
where NS is a symbol indicating the palette namespace of STYLE, which
can either be a symbol (resolved using styles defined with
gss-defstyle) or a lambda to directly pass a stylefun. :spec also
accepts an alist containing multiple style specs. Multiple :spec
kwargs can be passed, and later definitions override matching earlier
ones. "
  (cond ((not (symbolp palette)) (signal 'gss-bad-parse (list "palette definition must be a symbol")))
        ((null specs) (signal 'gss-bad-definition (list "defpalette with empty style specs")))
        ((get palette 'gss-palette) (signal 'gss-bad-definition (list (format "palette %s already defined" palette))))
        ((not (plistp specs)) (signal 'gss-bad-parse (list "spec arguments must be a plist")))
        (t nil))
  (condition-case err
      (cl-flet ((improper-consp (cell)
                  ;; match a sole dotted pair (a . b) meant to be an alist member
                  (and (consp cell) (not (consp (cdr cell)))))
                (parse-style (style-spec)
                     ;; parse a style spec (ns . style) where ns is a namespace symbol and style is either a lambda or a symbol key in gss--styles
                     (pcase style-spec
                       (`(,(and (pred symbolp) ns) . ,(and (pred symbolp) style))
                        (if-let ((stylefun (alist-get style gss--styles)))
                            (setf (alist-get ns (get palette 'gss-spec)) stylefun)
                          (signal 'gss-bad-definition (list (format "Undefined style %s" style)))))
                       (`(,(and (pred symbolp) ns) . ,(and (pred functionp) style))
                        (setf (alist-get ns (get palette 'gss-spec)) stylefun))
                       (_ (signal 'gss-bad-parse (list (format  "Unknown style spec: %s" style-spec)))))))
        (cl-loop for arg on specs by #'cddr
                 do (pcase arg
                      (`(:style ,(and (pred improper-consp) style)) (parse-style style))
                      (`(:style ,(and (pred listp) styles)) (mapc parse-style styles))
                      (`(,prop _) (signal 'gss-bad-parse (list (format "Unknown kwarg %s" prop))))
                      (styledef (signal 'gss-bad-parse (list (format "Unknown style spec %s" styledef)))))))
    ;; Remove all gss-* symbol props before rethrowing so defpalette doesn't partially initialize a symbol
    ;; Note: This rethrow does not preserve the original stack trace, for that behavior use handler-bind instead of condition-case
    (error (cl-remprop palette 'gss-spec)
           (signal (car err) (cdr err)))
    (:success
     (put palette 'gss-palette t))))

;; Note: gss--current-palette is initialized with a default value to ensure Emacs permanently marks it as a special (dynamically scoped) variable everywhere instead of just locally in this file. However, we don't want it to be bound globally, only within (gss-with ...) forms or explicitly in a let form, hence the following call to makunbound, which clears the value but preserves its special status.
(defvar gss--current-palette nil)
(makunbound 'gss--current-palette)

(defmacro gss-with (palette &rest body)
  `(let ((gss--current-palette ,palette))
     (let-alist (get gss--current-palette 'gss-styles)
       ,@body)))

;;; Tokens

(defconst gss--forbidden-tokens '(token style face alias palette) "This list contains token names that would shadow existing gss-def* functions")
(setq gss--tokens nil)
(cl-defmacro gss-deftoken (token)
  "Define a GSS token and its constructor function \"gss-def<token>\""
  `(progn
     (cond ((memq ',token gss--tokens) (signal 'gss-bad-definition (format  "token \"%s\" already defined" ',token)))
           ((memq ',token gss--forbidden-tokens) (signal 'gss-bad-definition (format  "token \"%s\" cannot be one of %s" ',token gss--forbidden-tokens)))
           (t  (push ',token gss--tokens)))
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
  (pcase (cons key fun)
    ((guard (assq key gss--styles)) (signal 'gss-bad-definition (list (format  "style \"%s\" already defined" key))))
    (`(,(pred symbolp) . ,(pred functionp)) (setf (alist-get key gss--styles) fun))
    (_ (signal 'gss-bad-parse (list "Registered style functions are a lambda form taking a spec type and spec val as arguments with a quoted name")))))

(defun gss--compute-styles (palette)
  ;; NOTE: this is currently "embarrassingly parallel" but if we add the ability
  ;; for tokens to depend on other tokens then the token loop must be ordered.
  (pcase-dolist (`(,name ,token) (get gss--current-palette 'gss-tokens))
    (pcase-dolist (`(,ns ,style) (get gss--current-palette 'gss-spec))
      ;; if the style function returns a non-nil spec value, we assign it to a unique face symbol 
      (if-let* ((spec (funcall style token))
                ;; NOTE: it's tempting to use an uninterned symbol here but I am not sure how that interacts
                ;; with the C-level implementation of face specs, which (I believe) assumes all face symbols
                ;; are interned for the entire runtime of Emacs.
                (face-sym (gss-symcat palette ns type name intern)))
          (progn
            (defface face-sym spec "GSS Internal Style Def")
            (setf (alist-get name (get 'gss-styles gss--current-palette)) face-sym))))))

(defun gss-refresh (&rest palettes)
  (mapc #'gss--compute-styles palette))

;;; Style Functions

(defmacro gss-set (face &rest styles)
  `(progn (face-spec-reset-face ,face)
          (set-face-attribute ,face nil :inherit (list ,@styles))))

(defmacro gss-defface (face doc &rest styles)
  `(progn (defface ,face nil ,doc)
          (gss-set ',face ,@styles)))

(defmacro gss-defalias (name &rest styles)
  "Define a single style name that refers to the styles passed. Aliases may not overwrite style names specified by the palette."
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
  ;; (defstyle foo .foo) ;this changes nothing, .foo is already .existing.thing
  `(if (assq ,name (get gss--current-palette 'gss-spec))
       (error "%s is already a style name" ,name)
     (let ((face-sym (gss--symcat gs--current-palette name)))
       (gss-defface face-sym "GSS Internal Style Alias" ,@styles)
       (setf (alist-get name (get 'gss-styles gs--current-palette)) face-sym))))

(provide 'gss)
