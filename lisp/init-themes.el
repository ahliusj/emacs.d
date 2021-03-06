;; (require-package 'monokai-theme)
;; (setq-default custom-enabled-themes '(monokai))

;; ;; Ensure that themes will be applied even if they have not been customized
;; (defun reapply-themes ()
;;   "Forcibly load the themes listed in `custom-enabled-themes'."
;;   (dolist (theme custom-enabled-themes)
;;     (unless (custom-theme-p theme)
;;       (load-theme theme)))
;;   (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

;; (add-hook 'after-init-hook 'reapply-themes)

;; (load-theme 'monokai t)
;; ;; override specific core colors
;; (setq ;; foreground and background
;;  monokai-foreground     "#ABB2BF"
;;  monokai-background     "#282C34"
;;  ;; highlights and comments
;;  monokai-comments       "#F8F8F0"
;;  monokai-emphasis       "#282C34"
;;  monokai-highlight      "#FFB269"
;;  monokai-highlight-alt  "#66D9EF"
;;  monokai-highlight-line "#1B1D1E"
;;  monokai-line-number    "#F8F8F0"
;;  ;; colours
;;  monokai-blue           "#61AFEF"
;;  monokai-cyan           "#56B6C2"
;;  monokai-green          "#98C379"
;;  monokai-gray           "#3E4451"
;;  monokai-violet         "#C678DD"
;;  monokai-red            "#E06C75"
;;  monokai-orange         "#D19A66"
;;  monokai-yellow         "#E5C07B")

;; ;; change the font size of your org-mode headers
;; (setq monokai-height-minus-1 0.8
;;       monokai-height-plus-1 1.1
;;       monokai-height-plus-2 1.15
;;       monokai-height-plus-3 1.2
;;       monokai-height-plus-4 1.3)

(require-package 'color-theme-sanityinc-solarized)
(require-package 'color-theme-sanityinc-tomorrow)

;; If you don't customize it, this is the theme you get.
(setq-default custom-enabled-themes '(sanityinc-tomorrow-bright))

;; Ensure that themes will be applied even if they have not been customized
(defun reapply-themes ()
  "Forcibly load the themes listed in `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme)))
  (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes))))

(add-hook 'after-init-hook 'reapply-themes)


;;------------------------------------------------------------------------------
;; Toggle between light and dark
;;------------------------------------------------------------------------------
(defun light ()
  "Activate a light color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-day))
  (reapply-themes))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (setq custom-enabled-themes '(sanityinc-tomorrow-bright))
  (reapply-themes))


(when (maybe-require-package 'dimmer)
  (setq-default dimmer-fraction 0.15)
  (add-hook 'after-init-hook 'dimmer-mode))


(provide 'init-themes)
