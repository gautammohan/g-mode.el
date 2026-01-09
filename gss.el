
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

(defun gss--symcat (&rest symbols)
  (intern (string-join (mapcar #'symbol-name symbols) "-")))

(cl-defmacro gss-defpalette (palette &keyword (prefix "gss--palette-") &rest specs)
  "Define a new palette with associated style specification.
Each style spec (specified using :spec) is a cons cell (NS . STYLE)
where NS is a symbol indicating the palette namespace of STYLE, which
can either be a symbol (resolved using styles defined with
gss-defstyle) or a lambda to directly pass a stylefun. :spec also
accepts an alist containing multiple style specs. Multiple :spec
kwargs can be passed, and later definitions override matching earlier
ones. "
  `(when (plistp ',specs)
     (let ((parse (lambda (style-spec)
                    ;; parse a style spec (ns . style) where ns is a namespace symbol and style is either a lambda or a symbol key in gss--styles
                    (pcase style-spec
                      (`(,((and (pred symbolp) ns) . ,(and (pred symbolp) style)))
                       (if-let ((stylefun  (alist-get style gss--styles)))
                           (setf (alist-get ns (get ',palette 'gss-spec)) stylefun)
                         (error "gss style %s not defined" style)))
                      (`(,((and (pred symbolp) ns) . ,(and (pred functionp) style)))
                       (setf (alist-get ns (get ',palette 'gss-spec)) stylefun))
                      (_ (error "Invalid gss style"))))))
       (cl-loop for arg on ',specs by #'cddr
                do (pcase arg
                     (`(:style ,(and (pred listp) styles)) (mapc parse styles))
                     (`(:style ,style) (funcall parse style))
                     (`(prop _) (warn "gss: ignoring unknown kwarg %s" prop)))))
     (put ',palette 'gss-palette t)))

;; must remain unbound globally and is only set implicitly within (gss-with ...) forms
(defvar gss--current-palette)
(defmacro gss-with (palette &rest body)
  `(let ((gss--current-palette ,palette))
     (let-alist (get gss--current-palette 'gss-styles)
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

(setq gss--styles nil "GSS registered styles, can be referred to by symbol in palette definitions")
(defun gss-defstyle (key fun)
  "A style function takes one argument, a style token, and generates a
  an Emacs defface-compliant facespec from it. Once defined it is
  available to all palettes definitions by its key symbol. To limit a
  style function to a single palette, use a lambda in the palette's
  spec definition."
  (pcase (list key fun)
    ((guard (memq key gss--styles)) (error "GSS style \"%s\" already registered" key))
    (`((pred symbolp) (pred functionp)) (setf (alist-get key gss--styles) fun))
    (_ (error "Registered style functions are a lambda form taking a spec
type and spec val as arguments with a quoted name"))))

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
