
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

(defconst gss-spec-types '(color attr))

;; define a spec fragment which will ultimately be merged into a complete face. type must be a value specified by gss-spec-types (TODO). if palette is nil explicitly then return the uninterned spec symbol itself. TODO - also deal with non-interned palette symbols
(cl-defmacro gss--defspec (name &key type gui tty (palette 'gss--global))
  `(let ((spec (gensym (format "%s:" (symbol-name ,type)))))
     (put spec 'gui ,gui)
     (put spec 'tty ,tty)
     (put spec 'type ,type)
     (setf (alist-get ',name (alist-get ,type ,palette)) spec)))

;; Reify a gss spec fragment into an actual Emacs face-spec. each spec must have its own function that provides relevant information.

;; reify all defined specs into base face defns and populate the palette namespace
(cl-defun gss--reify (spec face-sym context)
  ;; for each spec
  ;;   reify/spec
  ;;     if singleton: gen facename, set .style.<name> = (set-face-spec name spec)
  ;;     else: for each (name, spec), do the singleton thing.
  (let* ((reifier (intern (concat
                           "gss--reify/"
                           (symbol-name (get spec 'type)))))
         (result (funcall reifier spec context)))
    (if (hash-table-p result)
        (map-apply (lambda (id spec) (let ((face (intern (string-join (mapcar #'symbol-name (list face-sym id)) "-"))))
                                       (face-spec-set face spec)
                                       (cons id face)))
                   result)
      (progn (face-spec-set face-sym result)
             face-sym))))

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

(cl-defun gss--reify/attr (spec context)
  `((((type graphic)) . ,(get spec 'gui))
    (((type tty)) . ,(get spec 'tty))))

(cl-defun gss--setstyle (style palette)
  (setf (alist-get style (alist-get 'style palette))  style))

(cl-defun gss--update-palettes (context &key (palettes '(gss--global)))
  (cl-loop for palette in palettes do
           (cl-loop for type in gss-spec-types do
                    (cl-loop for (name . spec) in  (alist-get type (symbol-value palette))
                             for prefix = (intern (string-join (mapcar #'symbol-name (list palette type name)) "-"))
                             do (setf (alist-get name (alist-get type (alist-get 'style (symbol-value palette))))
                                      (gss--reify spec prefix context))))))

(cl-defmacro gss-with-palette (palette &rest body)
  `(cl-macrolet ((gss-set (face &rest styles)
                   `(progn (face-spec-reset-face ,face)
                           (set-face-attribute ,face nil :inherit (list ,@styles))))
                 (gss-defface (face doc &rest styles)
                   `(progn (defface ,face nil ,doc)
                           (gss-set ',face ,@styles)))
                 (gss-defstyle (face &rest styles)
                   `(progn (gss-defface ,face "GSS Internal Style Def" ,@styles)
                           (gss--setstyle ',face ',,palette))))
     (let-alist (alist-get 'style ,palette)
       ,@body)))

(cl-defmacro gss-set (&rest body)
  `(gss-with-palette gss--global (gss-set ,@body)))

(cl-defmacro gss-defface (&rest body)
  `(gss-with-palette gss--global (gss-defface ,@body)))

(cl-defmacro gss-defstyle (&rest body)
  `(gss-with-palette gss--global (gss-defstyle ,@body)))

(provide 'gss)
