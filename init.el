;;; init.el --- emacs configuration file with more pleasant defaults
;; Copyright (C) 1999-2015 Jeff Rose

;; Author: Jeff Rose
;; URL: https://github.com/xjdr/emacs.d
;; Version: 0.4

(setq user-full-name "Jeff Rose")
(setq user-mail-address "jeff.rose12@gmail.com")

;; Load site-lisp
(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
(add-hook 'after-init-hook '(lambda ()
															(load "~/.emacs.d/site-lisp/emacs-tile.el")
;															(load "~/.emacs.d/site-lisp/mode-line.el")
															(load "~/.emacs.d/site-lisp/google-c-style.el")
))

;; Lets get some packages
(setenv "PATH" (concat "/usr/local/bin:/opt/local/bin:/usr/bin:/bin" (getenv "PATH")))
(setenv "GOPATH" (concat (getenv "HOME") "/src/golang"))
(add-to-list 'exec-path (concat (getenv "GOPATH") "/bin"))
(require 'cl)

(load "package")
(package-initialize)
(add-to-list 'package-archives
						 '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
						 '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
						 '("elpa" . "http://tromey.com/elpa/") t)

(defvar xjdr/packages '(magit
												yaml-mode))

(defun xjdr/packages-installed-p ()
	(loop for pkg in xjdr/packages
				when (not (package-installed-p pkg)) do (return nil)
				finally (return t)))

(unless (xjdr/packages-installed-p)
	(message "%s" "Refreshing package database...")
	(package-refresh-contents)
	(dolist (pkg xjdr/packages)
		(when (not (package-installed-p pkg))
			(package-install pkg))))

;; Finally, lets set up some emacs
(setq inhibit-splash-screen t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)
(set-fringe-mode 0)
(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)
(setq-default indicate-empty-lines t)
(setq echo-keystrokes 0.1
			use-dialog-box nil
			visible-bell t)
(show-paren-mode t)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(global-font-lock-mode t)
(setq font-lock-maximum-decoration t)
(show-paren-mode t)
(setq backup-inhibited t)
(setq auto-save-default nil)

(when (not indicate-empty-lines)
	(toggle-indicate-empty-lines))

(setq tab-width 2
			indent-tabs-mode nil)

(setq make-backup-files nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;; Custom key bindings
(global-set-key (kbd "M-RET") 'toggle-frame-fullscreen)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)

;; change the filename collisions in emacs
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'ido)
(ido-mode 'both)

(require 'ido-vertical-mode)
(ido-vertical-mode)

(require 'smex)
(global-set-key (kbd "M-x") 'smex)

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(add-hook 'dired-load-hook
					(function (lambda () (load "dired-x"))))

;; c++ 
(defun my-c++-mode-hook ()
	(google-set-c-style)
	(google-make-newline-indent))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)

;; python
(add-hook 'python-mode-hook
					(lambda () 
						(setq indent-tabs-mode t)
						(setq python-indent 2)
						(setq tab-width 2))
						(tabify (point-min) (point-max)))

;; java
(add-hook 'java-mode-hook
					(lambda ()
						"Treat Java 1.5 @-style annotations as comments."
						(setq c-comment-start-regexp "(@|/(/|[*][*]?))")
						(modify-syntax-entry ?@ "< b" java-mode-syntax-table)
							(google-set-c-style)
							(google-make-newline-indent)))

;; YAML
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))

;; markdown
(require 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
	 "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))
(add-hook 'markdown-mode-hook
					(lambda ()
						(visual-line-mode t)
						(writegood-mode t)
						(flyspell-mode t)))

;; Dockerfile Support
(add-to-list 'load-path "/your/path/to/dockerfile-mode/")
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; Custom functions
(global-set-key (kbd "M-j") 
						(lambda ()
									(interactive)
									(join-line -1)))

(global-set-key (kbd "s-/")
								(lambda ()
									(interactive)
									(comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(global-set-key (kbd "C-o")
								(lambda ()
									(interactive)
									(previous-line)
									(end-of-line)
									(newline)))

;; mac specific settings
(when (eq system-type 'darwin)
	;(setq mac-option-modifier 'alt)
	;(setq mac-command-modifier 'meta)
	(global-set-key [kp-delete] 'delete-char) ;; sets fn-delete to be right-delete
	)

;; theme
;(if window-system
;		(progn
;			(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/solarized")
;			(customize-set-variable 'frame-background-mode 'light)
;			(load-theme 'solarized t)
;			(load "~/.emacs.d/site-lisp/mode-line-solarized-light.el"))
;	(progn
		(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
    (load-theme 'ir-black t)
		(load "~/.emacs.d/site-lisp/mode-line.el");))
