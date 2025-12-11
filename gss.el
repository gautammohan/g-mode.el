
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

(setq gss--global nil)
(defconst gss-spec-types '(color attr))

;; define a spec fragment which will ultimately be merged into a complete face. type must be a value specified by gss-spec-types (TODO). if palette is nil explicitly then return the uninterned spec symbol itself. TODO - also deal with non-interned palette symbols
(cl-defmacro gss--defspec (name &key type gui tty (palette 'gss--global))
  `(let ((spec (gensym (format "%s:" (symbol-name ,type)))))
     (put spec 'gui ,gui)
     (put spec 'tty ,tty)
     (put spec 'type ,type)
     (setf (alist-get ',name (alist-get ,type ,palette)) spec)))

;; Reify a gss spec fragment into an actual Emacs face-spec. each
;; defined spec type must provide its own implementation function as
;; gss--reify/<spec>, similar to the use-package convention.

(cl-defun gss--reify (spec face-sym context)
  ;; for each spec
  ;;   reify/spec
  ;;     if singleton: gen facename, set .style.<name> = (set-face-spec name spec)
  ;;     else: for each (name, spec), .name = (set-face-spec name spec)
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

;; Reify a font attribute directly
(cl-defun gss--reify/attr (spec context)
  `((((type graphic)) . ,(get spec 'gui))
    (((type tty)) . ,(get spec 'tty))))

;; Compute each spec as a face according to the context. The
;; namespace structure of each spec is cloned under .style in the palette
(cl-defun gss--update-palettes (context &key (palettes '(gss--global)))
  (cl-loop for palette in palettes do
           (cl-loop for type in gss-spec-types do
                    (cl-loop for (name . spec) in  (alist-get type (symbol-value palette))
                             for prefix = (intern (string-join (mapcar #'symbol-name (list palette type name)) "-"))
                             do (setf (alist-get name (alist-get type (alist-get 'style (symbol-value palette))))
                                      (gss--reify spec prefix context))))))

(cl-defun gss--setstyle (style palette)
  (setf (alist-get style (alist-get 'style palette))  style))

;; Provide a palette context to resolve style attribute definitions
;; for all gss-* functions in the body.
(cl-defmacro gss-with-palette (palette &rest body)
  "Use the provided palette to resolve style attributes for the gss
functions in the body."
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

;; Each of the global gss-* functions expands to gss-with-palette
;; pre-filled with the global palette. gss-with-palette contains
;; defined macros that shadow the gss-* names containing their definitions.

(cl-defmacro gss-set (&rest body)
  `(gss-with-palette gss--global (gss-set ,@body)))

(cl-defmacro gss-defface (&rest body)
  `(gss-with-palette gss--global (gss-defface ,@body)))

(cl-defmacro gss-defstyle (&rest body)
  `(gss-with-palette gss--global (gss-defstyle ,@body)))

(provide 'gss)
