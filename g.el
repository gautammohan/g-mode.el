;;; g.el --- Generic Mode-Based Overhaul

;; Author: Gautam Mohan <me@gautammohan.com>
;; Maintainers: Gautam Mohan <me@gautammohan.com>
;; Created: 2025-10-06
;; Version: 0.0.1
;; Keywords: emulations
;; URL: https://github.com/gautammohan/g-mode.el
;;; Commentary:

;; g.el or "Gmacs" is yet another overhaul of Emacs functionality similar to Spacemacs or Doom based on mneumonic, keyboard-driven modal interfaces.

;;; Code:

(defconst g-version "0.0.1")

(defgroup g nil
  "Gmacs Customizations"
  :group 'emacs)


(defface g-default nil
  "Gmacs default face")

(defun g--refresh-fonts (sym val)
  "This function will reset all gmacs faces dependent on a g-font-family variable.

NOTE: the variables are currently hardcoded in and this function must be updated whenever a new font family is added or another face redefines its font."
  (set-default-toplevel-value sym val)
  (set-face-attribute 'g-default nil :family g-font-family-monospaced)
  ())

(defcustom g-font-family-monospaced "Roboto Mono"
  "Monospaced font for gmacs, must have light/medium/bold weights and roman/italic styles"
  :type 'string
  :set 'g--refresh-fonts)

(defcustom g-font-family-proportional "Roboto"
  "Variable-pitch font for gmacs, must have light/medium/bold weights, roman/italic styles, and condensed/normal widths"
  :type 'string)


(provide 'g)

;;; g.el ends here
