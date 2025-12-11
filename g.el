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

(require 'gss)

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
    (hotaru-bi        . "#e7dc5f"))
  "Color values taken from Pilot's Iroshizuku Ink line + some custom additions")


(gss-defattr mono '(:family "Roboto Mono"))
(gss-defattr variable '(:family "Roboto"))
(gss-defattr emph '(:weight semi-bold))
(gss-defattr italic '(:slant italic))
(let-alist gss--default-colors
  (gss-defcolor text `(,.syun-gyo . ,.timberwolf))
  (gss-defcolor canvas `(,.ivory . ,.take-sumi)))
(setq ctx '((variant light)))
(gss--update-palettes ctx)

(provide 'g)

;;; g.el ends here
