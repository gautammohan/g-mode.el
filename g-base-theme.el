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

;; (custom-theme-set-faces 'g-base
;;                         `(default . ((default . (
;;                                                 :family ,(face-attribute 'g-default :family nil t)
;;                                                 :foundry nil
;;                                                 :width normal
;;                                                 :height ,(face-attribute 'g-default :height nil t)
;;                                                 :weight ,(face-attribute 'g-default :weight nil t)
;;                                                 :slant normal
;;                                                 :foreground ,(face-attribute 'g-default :foreground nil t)
;;                                                 :distant-foreground nil
;;                                                 :background ,(face-attribute 'g-default :background nil t)
;;                                                 :underline nil
;;                                                 :overline nil
;;                                                 :strike-through nil
;;                                                 :box nil
;;                                                 :inverse-video nil
;;                                                 :stipple nil
;;                                                 :inherit nil
;;                                                 :extend nil)))))


(custom-theme-set-faces 'g-base
                        g--default-face-override)
(provide-theme 'g-base)

