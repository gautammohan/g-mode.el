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

(defun g--theme-set-variant (variant)
  "Update all defined gmacs faces to the desired variant. "
  (message (format "Set g theme variant to %s" variant)))

(defcustom g-theme-active-variant 'light
  "Current gmacs theme variant"
  :type '(choice (const :tag "Light Mode" 'light)
                 (const :tag "Dark Mode" 'dark))
  :set (lambda (sym val)
         (set-default-toplevel-value sym val)
         (g--theme-set-variant val)))

(defun g-theme-toggle ()
  "Switch between light and dark gmacs theme variants"
  (interactive)
  (if (eq g-theme-active-variant 'light)
      (custom-set-variables
       '(g-theme-active-variant 'dark))
    (custom-set-variables
     '(g-theme-active-variant 'light))))

;; palettes are alists of color mappings whose color names have meaningful semantics wrt UI design components. 

;; variant palettes must be alists with identical keys which are a subset of the complete palette. actual faces only know about a single palette, whose values are merged with variants based on the active color scheme. They are hardcoded for now but could conceivably be generated from a macro if more variants are needed.

;; Polar Night: darkest to brightest
(defconst nord0 "#2E3440")
(defconst nord1 "#3B4252")
(defconst nord2 "#434C5E")
(defconst nord3 "#4C566A")

;; Snow Storm: darkest to brightest
(defconst nord4 "#D8DEE9")
(defconst nord5 "#E5E9F0")
(defconst nord6 "ECEFF4")

;; Frost: most-to-least contrasting
(defconst nord7 "#8FBCBB")
(defconst nord8 "#88C0D0")
(defconst nord9 "#81A1C1")
(defconst nord10 "#5E81AC")

;; Aurora: red, orange, yellow, green, purple
(defconst nord11 "#BF616A")
(defconst nord12 "#D08770")
(defconst nord13 "#EBCB8B")
(defconst nord14 "#A3BE8C")
(defconst nord15 "#B48EAD")

  ;; `((text . ,nord0)
  ;;   (text- . ,nord1)
  ;;   (text-- . ,nord2)
  ;;   (ui- . ,nord6)
  ;;   (ui . ,nord5)
  ;;   (ui+ . ,nord4))
(defconst g-face-attributes
  `((font . ((size . ,(defface g--attr-font-size '((default . (:height 160))) "Gmacs internals -- DO NOT EDIT"))
             (monospace . ,(defface g--attr-font-monospace '((default . (:family "Roboto Mono"))) "Gmacs internals -- DO NOT EDIT"))
             (proportional . ,(defface g--attr-font-proportional '((default . (:family "Roboto"))) "Gmacs internals -- DO NOT EDIT"))
             (italic . ,(defface g--attr-font-italic '((default . (:slant italic))) "Gmacs internals -- DO NOT EDIT"))
             (light . ,(defface g--attr-font-thin '((default . (:weight light))) "Gmacs internals -- DO NOT EDIT"))
             (regular . ,(defface g--attr-font-thin '((default . (:weight regular))) "Gmacs internals -- DO NOT EDIT")))
          )
    (color . ((text . ((base . ,(defface g--attr-text-base `((default . (:foreground ,nord0))) "Gmacs internals -- DO NOT EDIT"))
                       (subtle . ,(defface g--attr-text-subtle `((default . (:foreground ,nord1))) "Gmacs internals -- DO NOT EDIT"))
                       (diminished . ,(defface g--attr-text-base `((default . (:foreground ,nord2))) "Gmacs internals -- DO NOT EDIT"))))
              (ui . ((background . ,(defface g--attr-ui-background `((default . (:background ,nord6))) "Gmacs internals -- DO NOT EDIT"))
                     (subtle . ,(defface g--attr-ui-subtle `((default . (:background ,nord5))) "Gmacs internals -- DO NOT EDIT"))
                     (prominent . ,(defface g--attr-ui-prominent `((default . (:background ,nord4))) "Gmacs internals -- DO NOT EDIT"))))
              (accent . ((default . ,(defface g--attr-accent-default `((default . (:foreground ,nord7))) "Gmacs internals -- DO NOT EDIT"))
                         (prominent . ,(defface g--attr-accent-prominent `((default . (:foreground ,nord8))) "Gmacs internals -- DO NOT EDIT"))
                         (subtle . ,(defface g--attr-accent-subtle `((default . (:foreground ,nord9))) "Gmacs internals -- DO NOT EDIT"))
                         (diminished . ,(defface g--attr-accent-diminished `((default . (:foreground ,nord10))) "Gmacs internals -- DO NOT EDIT"))))
              (signal . ((error . ,(defface g--attr-signal-error `((default . (:foreground ,nord11))) "Gmacs internals -- DO NOT EDIT"))
                         (issue . ,(defface g--attr-signal-issue `((default . (:foreground ,nord12))) "Gmacs internals -- DO NOT EDIT"))
                         (warn . ,(defface g--attr-signal-warn `((default . (:foreground ,nord13))) "Gmacs internals -- DO NOT EDIT"))
                         (ok . ,(defface g--attr-signal-ok `((default . (:foreground ,nord14))) "Gmacs internals -- DO NOT EDIT"))
                         (alt . ,(defface g--attr-signal-alt `((default . (:foreground ,nord15))) "Gmacs internals -- DO NOT EDIT"))))))))


(defcustom g-palette-variant-light
  `((text . ,nord0)
    (text- . ,nord1)
    (text-- . ,nord2)
    (ui- . ,nord6)
    (ui . ,nord5)
    (ui+ . ,nord4))
  "Palette variants for light theme"
  :type '(alist
          :key-type (choice
                     (const :tag "Text - Base" text)
                     (const :tag "Text - Subtle" text-)
                     (const :tag "Text - Diminished" text-- color)
                     (const :tag "UI - Background" ui-)
                     (const :tag "UI - Subtle" ui)
                     (const :tag "UI - Prominent" ui+))
          :value-type color))

(defcustom g-palette-variant-dark
  `((text . ,nord6)
    (text- . ,nord5)
    (text-- . ,nord4)
    (ui- . ,nord0)
    (ui . ,nord1)
    (ui+ . ,nord2))
  "Palette variants for dark theme"
  :type '(alist
          :key-type (choice
                     (const :tag "Text - Base" text)
                     (const :tag "Text - Subtle" text-)
                     (const :tag "Text - Diminished" text-- color)
                     (const :tag "UI - Background" ui-)
                     (const :tag "UI - Subtle" ui)
                     (const :tag "UI - Prominent" ui+))
          :value-type color))

(defcustom g-palette (map-merge 'alist g-palette-variant-light
                                `((acc . ,nord7)
                                  (acc+ . ,nord8)
                                  (acc- . ,nord9)
                                  (acc-- . ,nord10)
                                  (error . ,nord11)
                                  (issue . ,nord12)
                                  (warn . ,nord13)
                                  (ok . ,nord14)
                                  (alt . ,nord15)))
  "Semantic color map used to define gmacs faces, defaults to light variant"

  :type '(alist
          :key-type (choice
                     (const :tag "Text - Base" text)
                     (const :tag "Text - Subtle" text-)
                     (const :tag "Text - Diminished" text-- color)
                     (const :tag "UI - Background" ui-)
                     (const :tag "UI - Subtle" ui)
                     (const :tag "UI - Prominent" ui+)
                     (const :tag "Accent - Default" acc)
                     (const :tag "Accent - Prominent" acc+)
                     (const :tag "Accent - Subtle" acc-)
                     (const :tag "Accent - Diminished" acc--)
                     (const :tag "Signal - Error" error)
                     (const :tag "Signal - Issue" issue)
                     (const :tag "Signal - Warn" warn)
                     (const :tag "Signal - Ok" ok)
                     (const :tag "Signal - Alt" alt) )
          :value-type color))

(provide 'g)

;;; g.el ends here
