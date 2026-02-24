
;;; grt.el --- Gmacs ERT Extensions


;; Author: Gautam Mohan <me@gautammohan.com>
;; Maintainers: Gautam Mohan <me@gautammohan.com>
;; Created: 2026-02-14
;; Version: 0.0.1
;; Keywords: testing
;; URL: https://github.com/gautammohan/g-mode.el
;;; Commentary:

;; This file provides useful combinators and test fixtures to simplify
;; writing ERT tests.

(require 'ert)

(cl-defun grt--restore-symbols (before after voidp)
  "Restore all symbol components to the values saved before test execution. The format of before and after is ((name . (:val <val> :fun <fun obj> :plist <plist))) for each 'name' specified in the grt symbol fixtures."
  (pcase-dolist (`(,sym . ,state) before)
    (let ((val (plist-get state :val))
          (fun (plist-get state :fun))
          (plist (plist-get state :plist)))
      (if (funcall voidp val)
          (makunbound sym)
        (set sym val))
      (if (funcall voidp fun)
          (fmakunbound sym)
        (fset sym fun))
      (setplist sym plist))))

(defun grt--assert-symbols-unchanged (before after _)
  (unless (equal before after)
    (signal 'error (list "Invariant failed: symbols are different" :before before :after after))))


(cl-defmacro grt-cleanup-symbols (names &rest rest &key (cleanup '#'grt--restore-symbols) &allow-other-keys)
  "Snapshot the value/function/plist components of every symbol in names, run the test body, running a cleanup function afterwards. The default cleanup function simply restores all the symbols to their original values.

 A custom cleanup function is passed 3 arguments (before after voidp). each of before/after is an alist holding each symbol name and a plist of their components: ((name . (:val <val> :fun <fun obj> :plist <plist>) ...)) for each name in names. voidp is a function to test if any of :val or :fun is unbound. see 'grt--restore-symbols' for an example cleanup function."
  (declare (indent 1))
  (cl-with-gensyms (void before after)
    `(let ((,void (make-symbol "void")))
       (cl-flet ((extract-state (sym)
                   (list
                    :val (if (boundp sym) (symbol-value sym) ,void)
                    :fun (if (fboundp sym) (symbol-function sym) ,void)
                    :plist (symbol-plist sym))))
         (let ((,before (mapcar (lambda (sym) (cons sym (extract-state sym))) ',names)))
           (unwind-protect
               ;; parsed :key val pairs specified by &key are included in &rest so the real body is
               (progn ,@(pcase rest
                          (`(:cleanup (,_ . ,body)) body)
                          (body body)))
             (let ((,after (mapcar (lambda (sym) (cons sym (extract-state sym))) ',names)))
               (funcall ,cleanup ,before ,after (lambda (val) (equal val ,void))))))))))

;; Bare bones sanity tests

(ert-deftest grt--test-cleanup-symbols-restores-value ()
  (set 'grt-test-sym 'original)
  (grt-cleanup-symbols (grt-test-sym)
    (set 'grt-test-sym 'modified))
  (should (eq (symbol-value 'grt-test-sym) 'original)))

(ert-deftest grt--test-cleanup-symbols-restores-function ()
  (fset 'grt-test-sym (lambda () 'original))
  (grt-cleanup-symbols (grt-test-sym)
    (fset 'grt-test-sym (lambda () 'modified)))
  (should (eq (funcall 'grt-test-sym) 'original)))

(ert-deftest grt--test-cleanup-symbols-restores-plist ()
  (setplist 'grt-test-sym '(:key original))
  (grt-cleanup-symbols (grt-test-sym)
    (setplist 'grt-test-sym '(:key modified)))
  (should (eq (get 'grt-test-sym :key) 'original)))

(provide 'grt)
