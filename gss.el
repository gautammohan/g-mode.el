
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

;;; Tokens

(defmacro gss--with-slots (&rest body)
  (declare (indent defun))
  "Anaphoric macro that binds `styles' `tokens' `faces' and `aliases' to setf-able locations corresponding to PALETTE"
  `(cl-symbol-macrolet ((styles  (get 'gss-styles gss--current-palette))
                        (tokens  (get 'gss-tokens gss--current-palette))
                        (faces   (get 'gss-faces gss--current-palette))
                        (aliases (get 'gss-aliases gss--current-palettes)))
     ,@body))

(defvar gss--tokens nil "GSS valid token types. Internal variable, do not edit manually.")
(defconst gss--forbidden-tokens '(style alias palette) "This list contains token names that would shadow existing gss functionality")

(cl-defmacro gss-deftoken (token &optional parsefun)
  (cond ((memq token gss--forbidden-tokens)
         (signal 'gss-bad-parse (format  "token \"%s\" cannot be one of %s" token gss--forbidden-tokens)))
        ((not (symbolp token))
         (signal 'gss-bad-parse (format  "token \"%s\" must be a symbol" token)))
        (t `(progn
              (add-to-list 'gss--tokens ',token)
              (defmacro ,(intern (format "gss-def%s" token)) (name val)
                `(if (symbolp ',name)
                     (gss--deftoken ',',token ',name (funcall #',',(or parsefun 'identity) ,','val))
                   (signal 'gss-bad-parse (format  "token \"%s\" must be a symbol" ,name))))))))

(cl-defun gss--deftoken (token name val)
  (gss--with-slots
    (when (memq name tokens)
      (signal 'gss-bad-definition (format "token \"%s\" already defined" name)))
    (setf (alist-get name slots) (cons token val))
    (gss--compute-faces :only-tokens (list name))))

(cl-defmacro gss-defstyle (namespace style-form)
  (unless (symbolp namespace)
    (signal 'gss-bad-parse "style namespace must be a symbol"))
  (let ((style-val (pcase style-form
                     (`(function  ,sym) `#',sym)
                     (`(lambda . ,_) `,style-form)
                     ((pred symbolp) ',style-form)
                     (_ (signal 'gss-bad-parse "style form must either be a symbol, lambda, or function symbol")))))
    `(gss--defstyle ',namespace ,style-val)))

(cl-defun gss--defstyle (namespace style)
  (gss--with-slots
    (when (memq namespace styles)
      (signal 'gss-bad-definition (format "style \"%s\" already defined" namespace)))
    (if (functionp style)
        (setf (alist-get namespace styles) style)
      (if-let (stylefun (alist-get style gss--styles))
          (setf (alist-get namespace styles) stylefun)
        (signal 'gss-bad-definition (list (format "Undefined style %s" style)))))
    (gss--compute-faces :only-styles (list namespace))))

(cl-defun gss-)

(cl-defmacro gss-defalias (name &rest refs)
  (unless (symbolp namespace)
    (signal 'gss-bad-parse "alias name must be a symbol"))
  (dolist (ref refs refs)
    (unless (symbolp ref)
      (signal 'gss-bad-parse (list (format "style ref must be a symbol: %s" styleref))))
    (unless (string-prefix-p "." (symbol-name ref))
      (signal 'gss-bad-parse (list (format "style ref must be preceded by '.': %s" styleref))))))

(cl-defun gss--parse-specs (styles aliases tokens args)
  "Recursively parse a plist of palette specs and return the parsed components. Unless all specs are parsed, this function will error."
  (cl-flet* ((unkeyword (sym)
               (intern (string-remove-prefix ":" (symbol-name sym))))
             (style-specp (spec)
               ;; Alias spec must be a list of symbols that start with '.'
               (dolist (styleref spec spec)
                 (unless (symbolp styleref)
                   (signal 'gss-bad-parse (list (format "style ref must be a symbol: %s" styleref))))
                 (unless (string-prefix-p "." (symbol-name styleref))
                   (signal 'gss-bad-parse (list (format "style ref must be preceded by '.': %s" styleref))))))
             (tokenp (kw) (and (keywordp kw)
                               (assq (unkeyword kw) gss--tokens))))
    (pcase args
      ;; no more args
      ('nil `(:style ,styles :alias ,aliases :tokens ,tokens))
      ;; Styles are either (name . style) for defined styles or (name . (lambda ..)) for custom stylefuns
      (`(:style (,(and (pred symbolp) name) . ,(and `(lambda . ,_) fun)) . ,remaining)
       (gss--parse-specs (cons `(',name . ,fun) styles) aliases tokens remaining))
      (`(:style (,(and (pred symbolp) name) . ,(and (pred symbolp) style)) . ,remaining)
       (gss--parse-specs (cons `(',name . ',style) styles) aliases tokens remaining))
      ;; Alias is (name . (.ref1 ... .refn))
      (`(:alias (,(and (pred symbolp) name) . ,(and (pred style-specp) spec)) . ,remaining)
       (gss--parse-specs styles (cons `(',name . ',spec) aliases) tokens remaining))
      ;; Any other valid specs (including defined tokens) must be of the format (:kw (name . <val>))
      (`(,(and (pred tokenp) key) (,(and (pred symbolp) name) . ,value) . ,remaining)
       (gss--parse-specs styles aliases (cons `(',(unkeyword key) (cons ',name ',value)) tokens) remaining))
      (_
       (signal 'gss-bad-parse (list (format "palette spec \"%s %s\" not recognized" (car args) (cadr args))))))))

(cl-defmacro gss-defpalette (palette &rest args)
  "Define a new palette with associated style specification.
Each style spec (specified using :spec) is a cons cell (NS . STYLE)
where NS is a symbol indicating the palette namespace of STYLE, which
can either be a symbol (resolved using styles defined with
gss-defstyle) or a lambda to directly pass a stylefun. :spec also
accepts an alist containing multiple style specs. Multiple :spec
kwargs can be passed, and later definitions override matching earlier
ones. "
  ``(gss--defpalette ,@,'(gss-parse-specs args)))

(cl-defun gss--compute-faces (&optional only-styles only-tokens)
  ;; recompute the given styles or all styles if args is nil
  (gss--with-palette
   (setq style-list (if only-styles
                        (mapcar (lambda (key) (assq key styles)))
                      styles)
         token-list (if only-tokens
                        (mapcar (lambda (key) (assq key tokens)))
                      tokens)))
    (pcase-dolist (`(,name . (,token . ,value)) token-list)
      (pcase-dolist (`(,ns . ,style) style-list)
        (if-let* ((spec (funcall style value))
                  ;; NOTE: it's tempting to use an uninterned symbol here but I am not sure how that interacts
                  ;; with the C-level implementation of face specs, which (I believe) assumes all face symbols
                  ;; are interned for the entire runtime of Emacs.
                  (face-sym (gss--symcat 'gss-- palette ns name)))
            (progn
              ;; should be face-spec-set because defface is a macro and we want a fn
              ;; otherwise face-sym will be a literal symbol instead of treated as avar
              (face-spec-set face-sym spec 'face-override-spec)
              (setf (alist-get name faces) face-sym))))))

(cl-defun gss--defstyle (palette namespace style)
  "Given a style fun and namespace, compute an alist whose keys are
.namespace.name and whose values are deffaces of the corresponding
style for each matching token type."
  (cl-symbol-macrolet ((styles (get palette 'gss-styles))
                       (faces  (get palette 'gss-faces)))
    (cond
     ((assq namespace (get 'gss-spec palette)) (signal 'gss-bad-definition (list (format "Style %s already defined" namespace))))
     ((functionp style) (setf (alist-get ns styles) style))
     ((symbolp style) (if-let ((stylefun (alist-get style gss--styles)))
                          (setf (alist-get ns styles) stylefun)
                        (signal 'gss-bad-definition (list (format "Undefined style %s" style)))))
     (t (signal 'gss-error "Catastrophic parse failure: undefined style form in gss-defpalette spec")))
  
    (gss--compute-faces palette style)))

(cl-defun gss--defalias (palette alias refs)
  (when (assq alias (get palette 'gss-aliases))
    (signal 'gss-bad-definition (list (format  "alias \"%s\" already defined" key))))
  (let-alist (get palette 'gss-faces)
    (cl-loop for ref in refs
             with inherit = nil
               do (if ref
                     (push ref inherit)
                   (signal 'gss-bad-definition (list (format "undefined alias ref \"%s\"" ref))))
             finally)))

(cl-defun gss--defpalette (palette (&key styles aliases tokens))
  "Successively build a palette given"
  (cond ((not (symbolp palette)) (signal 'gss-bad-parse (list "palette definition must be a symbol")))
        ((null specs) (signal 'gss-bad-definition (list "defpalette with empty style specs")))
        ((get palette 'gss-palette) (signal 'gss-bad-definition (list (format "palette %s already defined" palette))))
        (t nil))
  (condition-case err
      (progn
        (pcase-dolist (`(,token . (,name . ,val)) tokens)
          ;; .token.<name> = (<token> . (parse <val>)) where 'parse'
          ;; function is defined by deftoken
          (setf (alist-get name (alist-get 'token (get palette 'gss-spec)))
                `(,name . ,(funcall (alist-get token gss--tokens) val))))
        (pcase-dolist (`(,ns . ,style) styles)
          )
        (pcase-dolist (`(,name . ,refs) aliases)
          (dolist (ref refs (setf (alist-get name (get palette 'gss-aliases))))
            (unless (or (assq ref (get palette 'gss-styles))
                        (assq ref (get palette 'gss-aliases)))
              )
            )))
    ;; Remove all gss-* symbol props before rethrowing so defpalette doesn't partially initialize a symbol
    ;; Note: This rethrow does not preserve the original stack trace, for that behavior use handler-bind (Emacs >=30) instead of condition-case
    (error (cl-remprop palette 'gss-spec)
           (signal (car err) (cdr err)))
    (:success
     (put palette 'gss-palette t))))

;; Note: gss--current-palette is initialized with a default value to ensure Emacs permanently marks it as a special (dynamically scoped) variable everywhere instead of just locally in this file. However, we don't want it to be bound globally, only within (gss-with ...) forms or explicitly in a let form, hence the following call to makunbound, which clears the value but preserves its special status.
(defvar gss--current-palette nil)
(makunbound 'gss--current-palette)

(defmacro gss-with (palette &rest body)
  `(if-let ((gss--current-palette ,palette)
            (styles (get gss--current-palette 'gss-styles)))
       (let-alist styles
         ,@body)
     (signal 'gss-error (list "Invalid palette %s" ,palette))))
(cl-defmacro gss-deftoken (token)
  "Define a GSS token and its constructor function \"gss-def<token>\""
  `(progn
     
     (cl-defmacro ,(intern (concat "gss-def" (symbol-name token))) (name val)
       ;; Note: that "ugly" double quote+comma is necessary because we have a nested backquote. We want to return the quoted symbol stored in the variable 'token' passed in to the deftoken macro.
       `(gss--deftoken ',name ',',token ,val))))

;; store tokens in an alist under 'gss-tokens prop of palette symbol
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
                (face-sym (gss--symcat palette ns type name intern)))
          (progn
            ;; should be face-spec-set because defface is a macro and we want a fn
            ;; otherwise face-sym will be a literal symbol instead of treated as avar
            (face-spec-set face-sym spec 'face-override-spec)
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
