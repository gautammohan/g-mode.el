;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with C-x C-f and enter text in its buffer.

(require 'map)

(defconst gss--default-colors
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
    (hotaru-bi        . "#e7dc5f")))


(cl-defmacro gss-defspec (type default-selector)
  ;; define gss-def<type> args which expands to (gss-defspec type ,@args)
  ;; gss--type-default-
  `(cl-defmacro ,(intern (concat "gss-def" (symbol-name type))) (name &rest args)
       `(gss--defspec ,name ,,type ,@args))
  
  )


(defconst gss--global nil)

(defconst gss-spec-types '(color attr))

;; define a spec fragment which will ultimately be merged into a complete face. type must be a value specified by gss-spec-types (TODO). if palette is nil explicitly then return the uninterned spec symbol itself. TODO - also deal with non-interned palette symbols
(cl-defmacro gss--defspec (name &key type gui tty (palette 'gss--global))
  `(let ((spec (gensym (format "%s:" (symbol-name ,type)))))
     (put spec 'gui ,gui)
     (put spec 'tty ,tty)
     (put spec 'type ,type)
     (setf (alist-get ',name (alist-get ,type ,palette)) spec)))

;; color spec is (light . dark)
;; normally we'd separate tty/gui but that might change if we make palettes single-device as opposed to properly spec'd like current faces
(cl-defmacro gss-defcolor (name color &key (palette 'gss--global))
  `(gss--defspec ,name :type 'color :gui ,color :tty ,color :palette ,palette))

(cl-defmacro gss-defattr (name attr &key (palette 'gss--global))
  `(gss--defspec ,name :type 'attr :gui ,attr :tty ,attr :palette ,palette))

;; Reify a gss spec fragment into an actual Emacs face-spec. each spec must specify the information needed. it can return multiple values that will be in its namespace as a hash table, or a single value as the actual face spec. note that it MUST return a hash table not an alist as a singleton spec is itself an alist.

;; reify all defined specs into base face defns and populate the palette namespace
(cl-defun gss--reify (spec prefix context)
  (let* ((face-sym (lambda (&rest qualifiers)
                     (intern (string-join (cons (symbol-name prefix) qualifiers) "-"))))
         (reifier (intern (concat
                           "gss--reify/"
                           (symbol-name (get 'type spec)))))
         (result ((funcall reifier spec context))))
    ;; if singleton: gen facename, set .attr.<name> = (set-face-spec name spec)
    ;; else: for each (name, spec), do the singleton thing.
    (if (hash-table-p result)
        (map-apply (lambda (id spec)
                     `(,id . (face-spec-set ,(funcall face-sym id) ,spec))))
      (face-spec-set (face-sym) spec))))

;; variant is light or dark
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
         (result (cl-loop for (layer . name) in '((:foreground . fg) (:background . bg))
                          collect
                          `(,name .  (((type graphic) . (,layer ,@(funcall extract (get spec 'gui))))
                                      ((type tty) . (,layer ,@(funcall extract (get spec 'tty)))))))))
    (map-into result 'hash-table)))

(cl-defun gss--reify/attr (spec context)
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
gss--update-palettes

gss--update-palettes





(setq ctx '((variant . light)))
(gss--update-palettes 'ctx)

nil

nil
gss--global
((color (canvas . color:9062) (text . color:9061)) (attr (italic . attr:9060) (emph . attr:9059) (variable . attr:9058) (mono . attr:9057)))







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

(gss-defattr mono '(:family "Roboto Mono"))
(gss-defattr variable '(:family "Roboto"))
(gss-defattr emph '(:weight semi-bold))
(gss-defattr italic '(:slant italic))
(let-alist gss--default-colors
  (gss-defcolor text `(,.syun-gyo . ,.timberwolf))
  (gss-defcolor canvas `(,.ivory . ,.take-sumi)))


gss--global
((color (canvas . color:9062) (text . color:9061)) (attr (italic . attr:9060) (emph . attr:9059) (variable . attr:9058) (mono . attr:9057)))



(defmacro testme (name &rest rest)
  (format "%s, %s" name rest))
testme
(testme foo :bar baz :bax)
"foo, (:bar baz :bax)"

