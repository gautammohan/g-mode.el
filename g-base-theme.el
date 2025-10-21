;;; g-base-theme.el --- Gmacs Base Theme Defaults

;; Author: Gautam Mohan <me@gautammohan.com>
;; Maintainers: Gautam Mohan <me@gautammohan.com>
;; Created: 2025-10-17
;; Version: 0.0.1
;; Keywords: emulations, themes
;; URL: https://github.com/gautammohan/g-mode.el
;;; Commentary:

;; The Base theme contains face customizations for "core" Emacs parts like frames, default text bg/fg, minibuffers, child-frames, etc.

;;; Code:

(require 'g-faces)

(deftheme g-base "Gmacs base theme")

(custom-theme-set-faces 'g-base
                        `(default ,g--default-face-override)
                        `(region ,(g--use 'g-highlight)))
(provide-theme 'g-base)

