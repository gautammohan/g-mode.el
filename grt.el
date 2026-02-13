
;;; grt.el --- Gmacs ERT Combinators


;; Author: Gautam Mohan <me@gautammohan.com>
;; Maintainers: Gautam Mohan <me@gautammohan.com>
;; Created: 2026-02-14
;; Version: 0.0.1
;; Keywords: testing
;; URL: https://github.com/gautammohan/g-mode.el
;;; Commentary:

;; This file provides useful combinators and test fixtures to simplify
;; writing ERT tests.

(defmacro grt-eval-as-bytecode (&rest body)
  "Save body to a temp file, byte compile the file, then load it"
  (let ((file (gensym)))
    `(let ((,file (make-temp-file "temp-")))
       (unwind-protect
           (progn 
             (message "file: %s" ,file)
             (write-region (prin1-to-string '(progn ,@body)) nil ,file)
             (byte-compile ,file)
             (load-file ,file))))))

(defmacro grt-shadow-globals (vars &rest body)
  "For each global variable in vars, save its value and execute body within a let binding where all vars are reset to nil, restoring them to their original values once execution is finished."
  (declare (indent 1))
  (let ((prevs (mapcar #'gensym (mapcar #'symbol-name vars))))
    `(let* (,@(cl-mapcar #'list prevs vars)
            ,@(cl-mapcar #'list vars (make-list (length vars) nil)))
       (unwind-protect
           (progn
             ,@body)
         (setq ,@(cl-mapcan #'list vars prevs))))))
