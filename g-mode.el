(provide 'g-mode)

(defvar-keymap g-select-map
  :doc "Keymap for activating a g-mode."
  :suppress 'nodigits)

;; example:
;; "thing" with movement by word / sentence / paragraph / section
;; generate links to all sections
;; avy search, jump to all sentences
;; associated types: word/sentence/para/section

;; define a 'prose' algebraic effect which is a subtype of a 'thing' effect context with associated datatypes word/sentence/paragraph/section and operations that may feature them. 
;; 

things are types, simple specifications with :
sum type - alist of (variant . param) dotted pairs
product type - plist of (:field type) specifiers for typed records

use that to embed associated data with an effect
associated data can be used when in that effect context

(g-thing ((variant1 . (:field1 type1 :field2 type2))
         (variant2 . (:field1 type1 :field2 type2))))

(g-effect prose
          :type 'alg
          :data   'g-thing
          :operators   '((op1 arg)
                         (op2 arg1 arg2)))

(g-context 'eff1 'eff2)

;; by using a context, you import terms where evaluation interprets an effect. so anything with a g-xxx is automatically handled.

;; dont let effects data clash within the g-namespace, for now.
;; effects have associated data defined using sum/prod types
;; effects in a context give you g-op / g-data syntax defined by the effect itself
;; the context determines how it is interpreted by eval/apply.
;; so we write code to do things in context handlers. context statically pulls in effects which gives us access to g-xxx syntax for ops/eff data.
;; so in this sense, contexts function as an interpreter for eval/apply of composed DSLs defined by abstract effects.
;; if we go full interpreter, that means we basically have taken the standard elisp evaluator and lifted it into a particular context to give semantics to any g-expr. 
;; the g-maps are going to be interfaces to this DSL interpreter. You can map building expressions (kind of like re-builder) to a process of building structures via keyboard-driven transient map interfaces. these build abstract expressions. the selected g-context (or ad-hoc built context) serves as the interpreter for running things
;; we are basically augmenting the elisp eval/apply methodology by breaking it into modular composable abstractions. but at its heart it's still eval-apply which is a good fit for emacs paradigms I THINK/hope.


;; another: project
;; open terminal to directory
;; get default buffer
;; get proj buffers
;; grep within proj


(define-minor-mode g-mode "Toggle g-mode. When g-mode is enabled, it uses an emulation mode map to intercept all key commands and reinterpret them according to g-mode conventions. Additionally it loads, configures, and manages defined modes, contexts, queries, and visualizations."
  :global t
  :keymap (define-keymap "ESC" g-select-map)

  (add-to-list 'minor-mode-map-alist '(g-mode . g-mode-map))
  )

(keymap-set g-mode-select-map "e" #'evil-mode)




;;* 
