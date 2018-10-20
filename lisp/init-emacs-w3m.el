;;; w3m website
(require-package 'w3m)
(setq w3m-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8
      ;; emacs-w3m will test the ImageMagick support for png32
      ;; and create files named "png32:-" everywhere
      w3m-imagick-convert-program nil
      w3m-terminal-coding-system 'utf-8
      w3m-use-cookies t
      w3m-cookie-accept-bad-cookies t
      w3m-home-page "http://www.bing.com"
      w3m-command-arguments       '("-F" "-cookie")
      w3m-mailto-url-function     'compose-mail
      browse-url-browser-function 'w3m
      ;; use shr to view html mail which is dependent on libxml
      ;; I prefer w3m. That's emacs 24.3+ default setup.
      ;; If you prefer colored mail body and other advanced features,
      ;; you can either comment out below line and let Emacs decide the
      ;; best html mail rendering engine, or "(setq mm-text-html-renderer 'shr)"
      ;; in "~/.gnus.el"
      ;; mm-text-html-renderer 'w3m ; I prefer w3m
      w3m-use-toolbar t
      ;; show images in the browser
      ;; setq w3m-default-display-inline-images t
      ;; w3m-use-tab     nil
      w3m-confirm-leaving-secure-page nil
      w3m-search-default-engine "g"
      w3m-key-binding 'info)

(defun w3m-get-url-from-search-engine-alist (k l)
  (let (rlt)
    (if (listp l)
	(if (string= k (caar l))
	    (setq rlt (nth 1 (car l)))
	  (setq rlt (w3m-get-url-from-search-engine-alist k (cdr l)))))
        rlt))
;; C-u S g RET <search term> RET in w3m
(setq w3m-search-engine-alist
      '(("g" "http://www.bing.com/search?q=%s" utf-8)
	;; ;; stackoverflow search
	;; ("q" "http://www.google.com.au/search?q=%s+site:stackoverflow.com" utf-8)
	;; ;; elisp code search
	;; ("s" "http://www.google.com.au/search?q=%s+filetype:el"  utf-8)
	;; wikipedia
	("w" "http://en.wikipedia.org/wiki/Special:Search?search=%s" utf-8)
	;; online dictionary
	("d" "http://dictionary.reference.com/search?q=%s" utf-8)
	;; ;; java google search
	;; ("java" "https://www.google.com.au/search?q=java+%s" utf-8)
	;; ;; financial dictionary
	;; ("f" "http://financial-dictionary.thefreedictionary.com/%s" utf-8)
	;; ;; javascript search on mozilla.org
	;; ("j" "http://www.google.com.au/search?q=%s+site:developer.mozilla.org" utf-8)
	))

(defun w3m-set-url-from-search-engine-alist (k l url)
  (if (listp l)
      (if (string= k (caar l))
	  (setcdr (car l) (list url))
	(w3m-set-url-from-search-engine-alist k (cdr l) url))))

(defvar w3m-global-keyword nil
  "`w3m-display-hook' must search current buffer with this keyword twice if not nil")

(defun w3m-guess-keyword (&optional encode-space-with-plus)
  (unless (featurep 'w3m) (require 'w3m))
  (let* ((keyword (my-use-selected-string-or-ask "Enter keyword:"))
	 (encoded-keyword (w3m-url-encode-string (setq w3m-global-keyword keyword))))
    ;; some search requires plus sign to replace space
    (if encode-space-with-plus
	(replace-regexp-in-string "%20" " " encoded-keyword)
      encoded-keyword)))

(defun w3m-customized-search-api (search-engine &optional encode-space-with-plus)
  (unless (featurep 'w3m) (require 'w3m))
  (w3m-search search-engine (w3m-guess-keyword encode-space-with-plus)))

(defun w3m-stackoverflow-search ()
  (interactive)
  (w3m-customized-search-api "q"))

(defun w3m-java-search ()
  (interactive)
  (w3m-customized-search-api "java"))

(defun w3m-google-search ()
  "Google search keyword"
  (interactive)
  (w3m-customized-search-api "g"))

(defun w3m-google-by-filetype ()
  "Google search 'keyword filetype:file-extension'"
  (interactive)
  (unless (featurep 'w3m) (require 'w3m))
  (let ((old-url (w3m-get-url-from-search-engine-alist "s" w3m-search-engine-alist))
	new-url)
    ;; change the url to search current file type
    (when buffer-file-name
      (setq new-url (replace-regexp-in-string
		     "filetype:.*"
		     (concat "filetype:" (file-name-extension buffer-file-name))
		     old-url))
      (w3m-set-url-from-search-engine-alist "s" w3m-search-engine-alist new-url))
    (w3m-customized-search-api "s")
    ;; restore the default url
        (w3m-set-url-from-search-engine-alist "s" w3m-search-engine-alist old-url)))
(provide 'init-emacs-w3m)
;; init-emacs-w3m.el end here
