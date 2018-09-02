;; -*- lexical-binding: t -*-
(setq debug-on-error t)

;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

(let ((minver "24.3"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "24.5")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;;----------------------------------------------------------------------------
;; Adjust garbage collection thresholds during startup, and thereafter
;;----------------------------------------------------------------------------
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-preload-local.el"
;;----------------------------------------------------------------------------
(require 'init-preload-local nil t)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require-package 'wgrep)     ;; wgrep.el --- Writable grep buffer and apply the changes to files
(require-package 'diminish)  ;; diminish.el --- Diminished modes are minor modes with no modeline display
(require-package 'scratch)
(require-package 'command-log-mode)

(require 'init-frame-hooks)
(require 'init-xterm)     ;; require init-frame-hooks 
(require 'init-themes)    ;;
;;;; lines with four ';' were set by ahliusj
;;;; (require 'init-osx-keys)  ;; for Mac os user
;;;; (require 'init-gui-frames);; stop C-z from minimizing windows under OS X 
(require 'init-dired)     ;; from emacs wiki: makes an Emacs buffer containing a listing of directory, and optionally some of its subdirectories as well.
(require 'init-isearch)   ;; show number of matches while searching
(require 'init-grep)
(require 'init-uniquify)  ;; nicer naming of buffers for files with identical names
(require 'init-ibuffer)   ;; buffer set up
(require 'init-flycheck)  ;; Syntax checking. support limitted language, see 'www.flycheck.org/en/latest'

(require 'init-recentf)   ;; organize recent files
(require 'init-smex)      ;; 
(require 'init-ivy)       ;; an interactive interface for completion
;; (require 'init-helm)      ;; similar to ivy, incremental completions and narrowing seletions
(require 'init-hippie-expand)           ;; a single command providing a variety of completions ans expansions. See Emacs WiKi.
(require 'init-company)                 ;; stands for "complete anything"
(require 'init-windows)                 ;; emacs frame, called windows, setting
(require 'init-sessions)                ;; save a list of open files in .emacs.desktop
(require 'init-fonts)                   ;; 
(require 'init-mmm)                     ;; multiple major modes

(require 'init-editing-utils)           ;; some setting, huge file
(require 'init-whitespace)              ;; make white-spaces visible

(require 'init-vc)                      ;; 
(require 'init-darcs)                   ;; 
(require 'init-git)                     ;; need init-vc
(require 'init-github)

(require 'init-projectile)              ;; 

(require 'init-compile)                 ;; 
;;(require 'init-crontab)
;; ;; (require 'init-textile)
;; ;; (require 'init-markdown)
;; ;; (require 'init-csv)
;; ;; (require 'init-erlang)
;; ;; (require 'init-javascript)
;; ;; (require 'init-php)
;; ;; (require 'init-org)
;; ;; (require 'init-nxml)
;; ;; (require 'init-html)
;; ;; (require 'init-css)
;; ;; (require 'init-haml)
;; ;; (require 'init-http)
;; ;; (require 'init-python)
;; ;; (require 'init-haskell)
;; ;; (require 'init-elm)
;; ;; (require 'init-purescript)
;; ;; (require 'init-ruby)
;; ;; (require 'init-rails)
;; ;; (require 'init-sql)
;; ;; (require 'init-rust)
;; ;; (require 'init-toml)
;; ;; (require 'init-yaml)
;; ;; (require 'init-docker)
;; ;; (require 'init-terraform)
;; ;; (require 'init-nix)
;; ;; (maybe-require-package 'nginx-mode)

(require 'init-paredit)         ;; keep parentheses balanced
(require 'init-lisp)                    ;; 'Happy hacking ...' is in this file
;; ;; (require 'init-slime)                   ;; Superior Lisp Interaction Mode, Enhanced
;; ;; (require 'init-clojure)                 ;; clojure is a kind of Lisp language
;; ;; (require 'init-clojure-cider)           ;; Clojure Interactive Development Environment that Rocks (CIDER)
;; ;; (require 'init-common-lisp)

(when *spell-check-support-enabled*
  (require 'init-spelling))

(require 'init-misc)                    ;; MISCellaneous file operation

(require 'init-folding)                 ;; folding
(require 'init-dash)                    ;; 

;;(require 'init-twitter)
;; (require 'init-mu)
;; ;; (require 'init-ledger)                  ;; a command line accounting tool 
;; Extra packages which don't require any configuration

(require-package 'gnuplot)
;; ;; (require-package 'lua-mode)
;; ;; (require-package 'htmlize)              ;; onverts the buffer text and the associated decorations to HTML
;; ;; (require-package 'dsvn)                 ;; This is an interface for managing Subversion working copies.  It can show you an up-to-date view of the current status, and commit changes. If also helps you do other tasks such as updating, switching, diffing and more.
(when *is-a-mac*
  (require-package 'osx-location))
(unless (eq system-type 'windows-nt)
  (maybe-require-package 'daemons))
(maybe-require-package 'dotenv-mode)

(when (maybe-require-package 'uptimes)
  (setq-default uptimes-keep-count 200)
  (add-hook 'after-init-hook (lambda () (require 'uptimes))))


;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))

;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))


;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)


;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
(require 'init-local nil t)



(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
