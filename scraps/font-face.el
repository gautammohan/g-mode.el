
(require 'cl-lib)

;; Polar Night: darkest to brightest
(defconst nord0 "#2E3440")
(defconst nord1 "#3B4252")
(defconst nord2 "#434C5E")
(defconst nord3 "#4C566A")

;; Snow Storm (w/custom white): darkest to brightest
(defconst nord4 "#D8DEE9")
(defconst nord5 "#E5E9F0")
(defconst nord6 "#ECEFF4")
(defconst nordw "#F8FAFC")

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

(defconst g-face-attributes
  `((font . ((size . ,(defface g--attr-font-size '((default . (:height 160))) "Gmacs internals -- DO NOT EDIT"))
             (monospace . ,(defface g--attr-font-monospace '((default . (:family "Roboto Mono"))) "Gmacs internals -- DO NOT EDIT"))
             (proportional . ,(defface g--attr-font-proportional '((default . (:family "Roboto"))) "Gmacs internals -- DO NOT EDIT"))
             (straight . ,(defface g--attr-font-straight '((default . (:slant normal))) "Gmacs internals -- DO NOT EDIT"))
             (italic . ,(defface g--attr-font-italic '((default . (:slant italic))) "Gmacs internals -- DO NOT EDIT"))
             (regular . ,(defface g--attr-font-regular '((default . (:weight light))) "Gmacs internals -- DO NOT EDIT"))
             (emph . ,(defface g--attr-font-emph '((default . (:weight regular))) "Gmacs internals -- DO NOT EDIT"))
             (normal . ,(defface g--attr-font-normal '((default . (:width regular))) "Gmacs internals -- DO NOT EDIT"))))
    (color . ((text . ((base . ,(defface g--attr-text-base `((default . (:foreground ,nord0))) "Gmacs internals -- DO NOT EDIT"))
                       (secondary . ,(defface g--attr-text-secondary `((default . (:foreground ,nord1))) "Gmacs internals -- DO NOT EDIT"))
                       (tertiary . ,(defface g--attr-text-tertiary `((default . (:foreground ,nord2))) "Gmacs internals -- DO NOT EDIT"))
                       (quarternary . ,(defface g--attr-text-quarternary `((default . (:foreground ,nord3))) "Gmacs internals -- DO NOT EDIT"))))
              (ui . ((background . ,(defface g--attr-ui-background `((default . (:background ,nordw))) "Gmacs internals -- DO NOT EDIT"))
                     (secondary . ,(defface g--attr-ui-secondary `((default . (:background ,nord6))) "Gmacs internals -- DO NOT EDIT"))
                     (tertiary . ,(defface g--attr-ui-tertiary `((default . (:background ,nord5))) "Gmacs internals -- DO NOT EDIT"))
                     (quarternary . ,(defface g--attr-ui-quarternary `((default . (:background ,nord4))) "Gmacs internals -- DO NOT EDIT"))))
              (accent . ((default . ,(defface g--attr-accent-default `((default . (:foreground ,nord7))) "Gmacs internals -- DO NOT EDIT"))
                         (secondary . ,(defface g--attr-accent-secondary `((default . (:foreground ,nord8))) "Gmacs internals -- DO NOT EDIT"))
                         (tertiary . ,(defface g--attr-accent-tertiary `((default . (:foreground ,nord9))) "Gmacs internals -- DO NOT EDIT"))
                         (quarternary . ,(defface g--attr-accent-quarternary `((default . (:foreground ,nord10))) "Gmacs internals -- DO NOT EDIT"))))
              (signal . ((error . ,(defface g--attr-signal-error `((default . (:foreground ,nord11))) "Gmacs internals -- DO NOT EDIT"))
                         (issue . ,(defface g--attr-signal-issue `((default . (:foreground ,nord12))) "Gmacs internals -- DO NOT EDIT"))
                         (warn . ,(defface g--attr-signal-warn `((default . (:foreground ,nord13))) "Gmacs internals -- DO NOT EDIT"))
                         (ok . ,(defface g--attr-signal-ok `((default . (:foreground ,nord14))) "Gmacs internals -- DO NOT EDIT"))
                         (alt . ,(defface g--attr-signal-alt `((default . (:foreground ,nord15))) "Gmacs internals -- DO NOT EDIT"))))))))

(let-alist g-face-attributes
  (defface g-default `((default . (
                                   :inherit (,.font.size
                                             ,.font.monospace
                                             ,.font.regular
                                             ,.font.straight
                                             ,.font.normal
                                             ,.color.text.base
                                             ,.color.ui.background)
                                   :foundry nil
                                   :underline nil
                                   :strike-through nil
                                   :box nil
                                   :inverse-video nil
                                   :stipple nil
                                   :extend nil
                                   ))) "Default props for frame text")
  (defface g-highlight `((default . (:inherit (,.color.ui.secondary g-default-text)))) "Highlight background shade")
  (defface g-lowlight `((default . (:inherit (,.color.ui.tertiary g-default-text)))) "Dimmer Highlight"))

(defun g--face-set-1 ()
  (cl-loop for (attr . name) in face-attribute-name-alist do
           (set-face-attribute 'default nil attr (face-attribute 'g-default attr nil t))
           (redisplay)))

(defun g--face-set-2 ()
  (cl-loop for (attr . name) in face-attribute-name-alist do
           (set-face-attribute 'default nil attr (face-attribute 'g-default attr nil t))))

(defun g--face-set-3 ()
  (let ((attr-list (mapcan (lambda (attr) `(,attr ,(face-attribute 'g-default attr nil t))) '(:family :foundry))))
    (message (format "setting %s" attr-list))
    (apply #'set-face-attribute 'default nil attr-list)))

(defun g--face-set-4 ()
  (let ((attr-list (mapcan (lambda (attr) `(,attr ,(face-attribute 'g-default attr nil t))) '(:height :weight :slant :width))))
    (message (format "setting %s" attr-list))
    (apply #'set-face-attribute 'default nil attr-list)))

(defun g--face-set-5 ()
  (let ((attr-list (mapcan (lambda (attr) `(,attr ,(face-attribute 'g-default attr nil t))) '(:family :foundry))))
    (message (format "setting %s" attr-list))
    (apply #'set-face-attribute 'default nil attr-list))
  (redisplay)
  (let ((attr-list (mapcan (lambda (attr) `(,attr ,(face-attribute 'g-default attr nil t))) '(:height :weight :slant :width))))
    (message (format "setting %s" attr-list))
    (apply #'set-face-attribute 'default nil attr-list)))
