;;; -*- lexical-binding: t -*-
;;; There are two binding regimes refer to how varibles are looked up by their names: Lexical and dynamic binding.
;;; All variables names and their values live in one global table When DYNAMIC.
;;; When LEXICAL and each binding scope creats a new table of variable name and values,
;;; organized in a hierarchy called "the environment".

;;; This file was adapted from many configs, purcell's, redguardtoo's, and so on.
;;; The comments mainly come from the Emacs manual, Emacs lisp manual, and internet.
;;; Many thanks to the contributors!

;;; This variable determines whether the debugger is called when an error is signaled and not handled.
;;; If debug-on-error is t, all kinds of errors call the debugger, except those listed in debug-ignored-errors (see below).
;;; If it is nil, none call the debugger.  
(setq debug-on-error t)

;;; check OS type
(message "Operating System Type: %s" system-type)
;;; check Emacs version
(message "Emacs Version: %s" emacs-version)
;; (when (version< emacs-version "24.3"))

;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

(let ((minver "24.3"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))
(when (version< emacs-version "24.5")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade if possible."))

;;; https://www.gnu.org/software/emacs/manual/html_node/elisp/List-Variables.html
;;; =expand-file-name= this function converts filename to a absolute file name
;;; =user-emacs-directory= this varible holds the name of the =~/.emacs.d= directory
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
;;; recommended, since some customirized configuration will automatically be
;;; written to the custom-file, otherwise, will be here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
;; Calls (package-initialize)
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-preload-local.el"
;;----------------------------------------------------------------------------
(require 'init-preload-local nil t)	; no error if not found

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

;;; DIRED makes an Emacs buffer containing a listing of directory,
;;; and optionally some of its subdirectories as well.
;;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Dired.html
(require 'init-dired)
(require 'init-isearch)   ;; show number of matches while searching
(require 'init-grep)
;;; built-in function, making Buffer Names Unique.
;;; nicer naming of buffers for files with identical names
(require 'init-uniquify)  
;;; fullframe,
;;; ibuffer-vc, list buffers with their dirs. https://github.com/purcell/ibuffer-vc 
(require 'init-ibuffer)   ;; buffer set up
;; Syntax checking. support limitted language, see 'www.flycheck.org/en/latest'
(require 'init-flycheck)  

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;; shijie ;;;;;;;;;;;

;; show line number
(global-linum-mode t)

;;insert-current-date
(defun insert-current-date ()
  "Insert the current date"
  (interactive "*")
  ;; (insert (format-time-string "%Y/%m/%d %H:%M:%S" (current-time))))
  (insert (format-time-string "%Y/%m/%d" (current-time))))
(global-set-key "\C-xd" 'insert-current-date)

;;insert-current-time
(defun insert-current-time ()
  "Insert the current time"
  (interactive "*")
  ;; (insert (format-time-string "%Y/%m/%d %H:%M:%S" (current-time))))
  (insert (format-time-string "%H:%M:%S" (current-time))))
(global-set-key "\C-xt" 'insert-current-time)

(require 'init-latex)
;; TODO: yasnippet doesn't work well
;; ;; (require 'init-yasnippet)
(require 'init-org)   ;; comment now, use the primary org-mode
(require 'init-octave)
(require 'init-chinese)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(require 'init-crontab)
;; ;; (require 'init-textile)
;; ;; (require 'init-markdown)
;; ;; (require 'init-csv)
;; ;; (require 'init-erlang)
;; ;; (require 'init-javascript)
;; ;; (require 'init-php)

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

(require 'init-folding)                 ;; Origami, A text folding minor mode for Emacs. 
(require 'init-dash)                    ;; 

;;(require 'init-twitter)
;; (require 'init-mu)
;; ;; (require 'init-ledger)                  ;; a command line accounting tool 
;; Extra packages which don't require any configuration

(require-package 'gnuplot)
;; ;; (require-package 'lua-mode)
;;; onverts the buffer text and the associated decorations to HTML
;; ;; (require-package 'htmlize)              
;;; This is an interface for managing Subversion working copies.
;;; It can show you an up-to-date view of the current status, and commit changes.
;;; If also helps you do other tasks such as updating, switching, diffing and more.
;; ;; (require-package 'dsvn)                 
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
