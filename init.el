;; init.el --- emacs startup file -*- lexical-binding: t -*-
;; https://github.com/raxod502/straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; install use-package with straight.el
(straight-use-package 'use-package)
;; use-package: install with straight.el instead of package.el
;; https://github.com/raxod502/straight.el#integration-with-use-package-1
(setq straight-use-package-by-default t)
;; download gnu-elpa packages from github mirror straight-mirror/<package>
;; https://github.com/raxod502/straight.el#gnu-elpa
(setq straight-recipes-gnu-elpa-use-mirror t)

;; install org
(use-package org)

;; install tron theme and activate it
(use-package tron-legacy-theme
  :config
  (load-theme 'tron-legacy t))

(if (boundp 'mac-option-modifier)
      (progn
	(setq mac-option-modifier 'meta)
	(setq mac-command-modifier 'hyper)
	))

(defun pdex-expand-file (file-base ext)
  ;; look for file in the same directory as init.el
  (expand-file-name (concat file-base ext) (file-name-directory (or load-file-name (buffer-file-name)))))

(add-to-list 'load-path "~/.emacs.d/")

(defun pdex-org-init (file-base)
  (let ((path-to-org (pdex-expand-file file-base ".org"))
        (path-to-elc (pdex-expand-file file-base ".elc")))
    (if (file-newer-than-file-p path-to-org path-to-elc)
      (org-babel-load-file path-to-org t)
      (load path-to-elc nil nil t))))

(pdex-org-init "editorconfig")
(pdex-org-init "emacs-cheatsheet")
(pdex-org-init "org-theme")
;; TODO rename this to session
(pdex-org-init "shutdown")
(pdex-org-init "window")

;; tell customize to write it's state outside of init.el
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; load the customized state (if available)
(if (file-exists-p custom-file)
    (load custom-file))
