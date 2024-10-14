;; effects are defined as named structs
(cl-defstruct eff2)

;; effects have nominal, linear subtyping from single-inheritance of cl-structs
(cl-defstruct eff1)
(cl-defstruct my-eff1 (:include eff1))

;; effect syntax defined as tagged generic methods
(cl-defgeneric op1 ((eff1 eff1) (arg1 integer) (arg2 buffer))
  (message "default eff1-op1"))

(cl-defgeneric op2 ((eff2 eff2) (arg1 string) (argn2 subr))
  (message "default eff1-op2"))

;; interpreters can specialize effects using cl-struct subtyping
(cl-defmethod op1 ((eff1 my-eff1) (arg integer) (argn buffer))
  (message "specialized my-eff1 op")
  (cl-call-next-method) ;; and call supertype impls if necessary
  )

;; All the available effects are defined as a generic method whose arguments represent a row-polymorphic record of effects.

(cl-defgeneric interpreter ((handler eff1) (handler eff2))
  (message "context is a row-polymorphic record of available handlers for all effects"))

;; an interpreter is a specialization that defines the semantics of handling combinations of effects

(cl-defmethod interpreter ((eff1-handler eff1) (eff2-handler eff2))
  (message "this is the \"global\" interpreter, which should be empty as it specifies no behavior"))

;; interpreters can use dynamic dispatch to automatically handle subtyping within effects, and respects the hierarchy of resolving struct inheritance (most-to-least specific)

(cl-defmethod interpreter ((eff1-handler my-eff1) (eff2-handler eff2))
  (message "use my-eff1 for dispatching eff1 ops"))

;; an effects interpreter is a generic method that specifies how each effect is handled. For dynamic dispatch to work, all effects must be included in the arguments even if they are unused, but effects can be "turned off" by using a null-handler, and macros can enable syntactic sugar to hide "unused" arguments.

(cl-defmethod interpreter ((eff1-handler null) (eff2-handler eff2))
  (message "eff2 is handled here, eff1 is not"))

;; hypothetical (cl-defmacro def-interpreter 'eff2 ...) would autogenerate the above sig by implicitly ignoring effq

;; note that dynamic dispatch will build an effective method by calling all context definitions from most-to-least specialized. This provides us a structural subtyping semantics for the row of effects, since interpreters which specialize more effects are called first.

;; For the interpreter, (cl-call-next-method) represents the "rest of the computation" with all uninterpreted effects (hopefully) handled. In this sense, you can think of the effective method built by cl-defgeneric as continuation-passing to interpret the row of effect arguments.

;; the actual effect runner will attempt to execute the ctx function, starting with a set of uninterpreted effects. it will reapply/re-interpret each custom context, tracking which effects have been dispatched and which may have been added. if the fixpoint of this interpretation yields anything other than the empty set, it means some effects have not been interpreted and the computation is divergent.

;; downsides: fails the expression problem since all methods signatures must update when new effects are added, but this can be solved by hiding these implementations behind macros (g-make-effect) and (g-make-context) that will handle this via expansions.


;; So what exactly are the semantics for multiple dispatch with "overlapping" methods?
(cl-defstruct base)
(cl-defstruct (child (:include base)))
(setq child (make-child))

(cl-defmethod foo ((a base) (b base) (c base))
  (message "base"))
(cl-defmethod foo ((a child) (b base) (c base))
  (message "a spec")
  (cl-call-next-method))
(cl-defmethod foo ((a base) (b child) (c base))
  (message "b spec")
  (cl-call-next-method))
(cl-defmethod foo ((a base) (b base) (c child))
  (message "c spec")
  (cl-call-next-method))
(cl-defmethod foo ((a child) (b child) (c base))
  (message "a, b spec")
  (cl-call-next-method))
(cl-defmethod foo ((a child) (b base) (c child))
  (message "a, c spec")
  (cl-call-next-method))
(cl-defmethod foo ((a base) (b child) (c child))
  (message "b, c spec")
  (cl-call-next-method))
(cl-defmethod foo ((a child) (b child) (c child))
  (message "a, b, c spec")
  (cl-call-next-method))

(cl-defgeneric foo (a b c)
  (:argument-precedence-order a b c))
(foo child child child)

(cl-defgeneric foo (a b c)
  (:argument-precedence-order a c b))
(foo child child child)

(cl-defgeneric foo (a b c)
  (:argument-precedence-order b a c))
(foo child child child)

(cl-defgeneric foo (a b c)
  (:argument-precedence-order b c a))
(foo child child child)

(cl-defgeneric foo (a b c)
  (:argument-precedence-order c a b))
(foo child child child)

(cl-defgeneric foo (a b c)
  (:argument-precedence-order c b a))
(foo child child child)
