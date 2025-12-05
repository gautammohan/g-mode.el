
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

(defconst gss--default-palette
  `((ivory            . "#FFFFEB")
    (timberwolf       . "#D6D6D6")
    (kiri-same        . "#979797")
    (take-sumi        . "#363636")
    (yama-guri        . "#614130")
    (syun-gyo         . "#443031")
    (tsukushi         . "#744420")
    (ina-ho           . "#966618")
    (to-ro            . "#ed7c0d")
    (yu-yake          . "#f04820")
    (fuyu-gaki        . "#d84020")
    (momiji           . "#e34343")
    (hana-ikada       . "#f77f87")
    (kosumosu         . "#f5534b")
    (tsutsuji         . "#d03e66")
    (yama-budo        . "#942064")
    (murasaki-shikibu . "#8c54a4")
    (ajisai           . "#4762c2")
    (asa-gao          . "#005ad2")
    (shin-kai         . "#374073")
    (tsuki-yo         . "#3f7e9e")
    (ama-iro          . "#189bcb")
    (tsuyu-kusa       . "#255eae")
    (kon-peki         . "#156ab2")
    (rikka            . "#3a83b6")
    (ku-jaku          . "#187981")
    (syo-ro           . "#008880")
    (sui-gyoku        . "#2d8065")
    (shin-ryoku       . "#008a65")
    (chiku-rin        . "#90a527")
    (hotaru-bi        . "#e7dc5f"))
  "Color values taken from Pilot's Iroshizuku Ink line + some custom additions")


(defconst gss--spec nil)

(defconst gss-spec-types '(color style))

;; define a spec fragment which will ultimately be merged into a complete face. type must be a value specified by gss-spec-types (TODO). if palette is nil explicitly then return the uninterned spec symbol itself. TODO - also deal with non-interned palette symbols
(cl-defmacro gss--defspec (name &key type gui tty (palette 'gss--spec))
  `(let ((spec (gensym (format "%s:" (symbol-name ,type)))))
     (put spec 'gui ,gui)
     (put spec 'tty ,tty)
     (put spec 'type ,type)
     (setf (alist-get ',name (alist-get ,type ,palette)) spec)))

;; Reify a gss spec fragment into an actual Emacs face-spec. each spec must have its own function that provides relevant information.

;; layer - :foreground or :background, variant is light or dark
;; reify all defined specs into base face defns and populate the palette namespace
(cl-defun gss--reify (spec prefix context)
  ;; for each spec
  ;;   reify/spec
  ;;     if singleton: gen facename, set .style.<name> = (set-face-spec name spec)
  ;;     else: for each (name, spec), do the singleton thing.
  (let* ((face-sym (lambda (&rest qualifiers)
                     (intern (string-join (cons (symbol-name prefix) qualifiers) "-"))))
         (reifier (intern (concat
                           "gss--reify/"
                           (symbol-name (get 'type spec)))))
         (result ((funcall reifier spec context))))
    (if (hash-table-p result)
        (map-apply (lambda (id spec)
                     `(,id . (face-spec-set ,(funcall face-sym id) ,spec))))
      (face-spec-set ,(funcall face-sym) ,spec))))

;; layer - :foreground or :background, variant is light or dark
(cl-defun gss--reify/color (spec variant)
  (let* ((extract (cond
                   ((eq variant 'light) #'car)
                   ((eq variant 'dark) #'cdr)
                   (t (progn 
                        (display-warning 'gss
                                         (format "Unknown variant %s" variant)
                                         :warning)
                        #'car))))
         (result (cl-loop for (layer . name) in '((:foreground . fg) (:background . bg)) collect
                          `(,name .  (((type graphic) . (,layer ,@(funcall extract (get spec 'gui))))
                                      ((type tty) . (,layer ,@(funcall extract (get spec 'tty)))))))))
    (map-into result 'hash-table)))

(cl-defun gss--reify/style (spec)
  `(((type graphic) . ,(get spec 'gui))
    ((type tty) . ,(get spec 'tty))))

(cl-defun gss--update-palettes (context &key (palettes '(gss--global)))
  (cl-loop for palette in palettes
           for type in gss-spec-types
           for (name . spec) in (alist-get type palette)
           with prefix = (string-join (mapcar #'symbol-name (list palette type name)) "-")
           do (setf (alist-get name
                               (alist-get type
                                          (alist-get 'style palette)))
                    (gss--reify spec prefix context))))

(cl-defmacro gss-with-palette (palette &rest body)
  `(cl-macrolet ((gss-set (face &rest styles)
                   `(progn (face-spec-reset-face ,face)
                           (set-face-attribute ,face nil :inherit (list ,@styles))))
                 (gss-defface (face doc &rest styles)
                   `(progn (defface ,face nil ,doc)
                           (gss-set ,face ,@styles)))
                 (gss-defstyle (face &rest styles)
                   `(progn (gss-defface ,face nil "Internal Style Def")
                           (setf (alist-get ,face ,,palette) ,face))))
     (let-alist ,palette
       ,@body)))

(cl-defmacro gss-set (&rest body)
  `(gss-with-palette gss--global (gss-set ,@body)))

(cl-defmacro gss-defface (&rest body)
  `(gss-with-palette gss--global (gss-defface ,@body)))

(cl-defmacro gss-defstyle (&rest body)
  `(gss-with-palette gss--global (gss-defstyle ,@body)))



(defconst gss--variant 'light)

(defconst gss--attributes nil)

(cl-defmacro gss-group (name &key (colors nil) (styles nil) (relative nil))

  )
'(group ((color ((bar . blah) (baz . bloop)))
         (face . ((default . zap) (emph)))))
(gss-face 'foo :fg .bar :bg .baz :decor )

(defface g--default nil "Default Gmacs Face")

(defconst gss--colors nil)
(cl-defmacro gss-color (name light dark &key (palette gss--default-palette))
  `(let-alist ',palette
     (setf (alist-get 'light (alist-get ,name gss--colors)) ,light)
     (setf (alist-get 'dark (alist-get ,name gss--colors)) ,dark)))

(gss-color 'text-body .syun-gyo .timberwolf)
(gss-color 'text-subtle .ina-ho .kiri-same)
(gss-color 'ui-base .ivory .take-sumi)

(defconst gss--props nil)
(cl-defmacro gss-prop (group name face-attr)
  (setf (alist-get ,name (alist-get ,group gss--props)) ,face-attr))
(gss-prop 'font 'size '())

(cl-defmacro gss-face (name group &rest blah &key (fg nil) (bg nil))
  (format "name: %s group: %s blah: %s fg: %s bg: %s" name group blah fg bg))


;; Default theme colors use Nord
(defconst gss-attributes
  `((font . ((size . ,(defface gss--attr-font-size '((default . (:height 160))) "Gmacs internals -- DO NOT EDIT"))
             (monospace . ,(defface gss--attr-font-monospace '((default . (:family "Roboto Mono"))) "Gmacs internals -- DO NOT EDIT"))
             (proportional . ,(defface gss--attr-font-proportional '((default . (:family "Roboto"))) "Gmacs internals -- DO NOT EDIT"))
             (straight . ,(defface gss--attr-font-straight '((default . (:slant normal))) "Gmacs internals -- DO NOT EDIT"))
             (italic . ,(defface gss--attr-font-italic '((default . (:slant italic))) "Gmacs internals -- DO NOT EDIT"))
             (regular . ,(defface gss--attr-font-regular '((default . (:weight regular))) "Gmacs internals -- DO NOT EDIT"))
             (emph . ,(defface gss--attr-font-emph '((default . (:weight bold))) "Gmacs internals -- DO NOT EDIT"))
             (normal . ,(defface gss--attr-font-normal '((default . (:width regular))) "Gmacs internals -- DO NOT EDIT"))))
    (mod . ((extend . ,(defface gss--attr-mod-extend '((default . (:extend t))) "Gmacs internals -- DO NOT EDIT"))))))

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
