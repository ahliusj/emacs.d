;;; init-latex.el --- Emacs setup for LaTeX.
;;
;;  https://github.com/ahliusj/emacs.d
;;
;; Thanks for the works of
;; Bozhidar Batsov <bozhidar@batsov.com> "https://github.com/bbatsov/prelude"
;; and
;; XiangYu Guo  "https://github.com/xyguo/emacs.d.git"
;; and
;; zhengfaxiang  "https://github.com/zhengfaxiang/emacs.d.git"
;;

;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Nice defaults for the premium LaTeX editing mode auctex.

;;; Code:

(require-package 'auctex)
;; (require-package 'reftex)

(mapc (lambda (mode)
        (add-hook 'Latex-mode-hook mode))
      (list 'auto-fill-mode
            'Latex-math-node
            'turn-on-reftex
            'Tex-fold-mode
            'linum-mode
            'auto-complete-mode
            'autopair-mode
            'outline-minor-mode))
;; (add-hook 'LaTeX-mode-hook
;;           (lambda ()
;;             (setq ;;TeX-auto-untabify t     ; remove all tabs before saving
;;              TeX-engine 'xetex       ; use xelatex default
;;              TeX-show-compilation nil) ; display compilation windows
;; 	    (setq LaTeX-electric-left-right-brace t)
;;             (TeX-global-PDF-mode t)       ; PDF mode enable, not plain
;;             (setq TeX-save-query nil)
;;             (imenu-add-menubar-index)
;;             ;;(define-key LaTeX-mode-map (kbd "TAB") 'TeX-complete-symbol)
;;             ))

;; (add-hook 'L)

;; (use-package latex-extra
;;   :ensure t
;;   :after latex
;;   :init
;;   (add-hook 'LaTeX-mode-hook #'latex-extra-mode)
;;   :config
;;   (setq latex/override-font-map nil)
;;   (auto-fill-mode -1))
;; (use-package latex
;;   :ensure auctex
;;   :mode ("\\.tex\\'" . latex-mode)
;;   :bind ("C-S-f" . forward-whitespace)
;;   :init
;;   (add-hook 'LaTeX-mode-hook
;;             (lambda ()
;;               (prettify-symbols-mode)
;;               (LaTeX-math-mode)
;;               (turn-on-reftex)
;;               (reftex-isearch-minor-mode)
;;               (turn-off-auto-fill)))
;;   (add-hook 'LaTeX-mode-hook 'visual-line-mode)
;;   :config
;;   (setq TeX-save-query nil)
;;   (setq TeX-auto-save t)
;;   (setq TeX-parse-self t)       ;enable document parsing
;;   (setq-default TeX-master nil) ;make auctex aware of multi-file documents
;;   (setq reftex-plug-into-AUCTeX t)
;;   (setq TeX-PDF-mode t)
;;   (setq TeX-electric-escape t)
;;   (setq global-font-lock-mode t)
;;   (font-lock-add-keywords 'latex-mode
;;                           (list (list "\\(«\\(.+?\\|\n\\)\\)\\(+?\\)\\(»\\)"
;;                                       '(1 'font-latex-string-face t)
;;                                       '(2 'font-latex-string-face t)
;;                                       '(3 'font-latex-string-face t))))

;;   ;; Method for enabling forward and inverse search
;;   (setq TeX-source-correlate-method 'synctex)
;;   ;; inhibit the question to start a server process
;; (setq TeX-source-correlate-start-server t)

;; ;; use sumatra to view pdf
;; ;;
;; ;;
;; ;; http://stackoverflow.com/questions/14448606/sync-emacs-auctex-with-sumatra-pdf
;; ;; -set-color-range #fdf4c1 #282828
;; (setq TeX-view-program-list
;;       '(("Sumatra PDF" ("\"C:/Program Files/SumatraPDF/SumatraPDF.exe\" -reuse-instance"
;;                         (mode-io-correlate " -forward-search %b %n ") " %o"))))

;; (eval-after-load 'tex
;;   '(progn
;;      (assq-delete-all 'output-pdf TeX-view-program-selection)
;; (add-to-list 'TeX-view-program-selection '(output-pdf "Sumatra PDF"))))
;; ;; jump to source
;; (setq TeX-source-correlate-mode t)

;; (use-package reftex
;;   :after latex
;;   :ensure t
;;   :bind ("C-c =" . reftex-toc)
;;   :config
;;   (setq reftex-cite-prompt-optional-args t) ; Prompt for empty optional arguments in cite
;;   ;; https://www.gnu.org/software/emacs/manual/html_mono/reftex.html
;;   (setq reftex-enable-partial-scans t)
;;   (setq reftex-keep-temporary-buffers nil)
;;   (setq reftex-save-parse-info t)
;;   (setq reftex-trust-label-prefix '("fig:" "eq:"))
;;   (setq reftex-default-bibliography "C:/Users/Nasser/OneDrive/Bibliography/references-zot.bib"))
;; (use-package company-bibtex
;;   :ensure t
;;   :after latex
;;   :config
;;   (add-to-list 'company-backends 'company-bibtex)
;;   (setq company-bibtex-bibliography
;; '("C:/Users/Nasser/OneDrive/Bibliography/references-zot.bib")))


(provide 'init-latex)
;; init-latex.el end here
