;; Lets set up some emacs
(blink-cursor-mode 0)
(setq initial-scratch-message nil)
(setq inhibit-startup-message t)

;; Why
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 0)
(set-fringe-mode 0)
(setq ring-bell-function 'ignore)

(setq tramp-default-method "ssh")
(defalias 'yes-or-no-p 'y-or-n-p)

;; I prefer my backups sorted elsewhere:
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
      backup-by-copying       t  ; Don't de-link hard links
      version-control         t  ; Use version numbers on backups
      delete-old-versions     t  ; Automatically delete excess backups:
      kept-new-versions       5  ; how many of the newest versions to keep
      kept-old-versions       5) ; and how many of the old

(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Use spaces, not tabs for indentation:
(setq-default indent-tabs-mode nil)

;; Show trailing whitespaces:
(require 'whitespace)
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Highlight matching parens:
(show-paren-mode t)

;; Lets get some packages
(load "package")
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("elpy" . "https://jorgenschaefer.github.io/packages/"))

;; uncomment the next line if the package cache is corrupt
;;(package-refresh-contents)
(package-initialize)

; list the packages you want
(setq package-list '(magit
                     editorconfig
                     ido-vertical-mode
                     smex
                     ir-black-theme
                     google-c-style))

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; ido
(require 'ido)
(ido-mode 1)
;(ido-everywhere 1)

;; ido-vertical
(require 'ido-vertical-mode)
(ido-vertical-mode)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)
(setq ido-use-faces t)
(set-face-attribute 'ido-vertical-first-match-face nil
                    :background nil
                    :foreground "green")
(set-face-attribute 'ido-vertical-only-match-face nil
                    :background nil
                    :foreground nil)
(set-face-attribute 'ido-vertical-match-face nil
                    :foreground nil)

;; smex
(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(setq smex-save-file "~/.emacs.d/smex.save") ;; keep my ~/ clean

(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(require 'dired-x)
(add-hook 'dired-load-hook
          (function (lambda () (load "dired-x"))))

;; Ansi Term
(defvar my-term-shell "/usr/local/bin/bash")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

;; Custom key bindings
(global-set-key (kbd "s-<return>") 'toggle-frame-fullscreen)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "s-/") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-x t") 'ansi-term)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x c e") 'flymake-display-err-menu-for-current-line)
(global-set-key (kbd "C-x c n") 'flymake-goto-next-error)
(global-set-key (kbd "C-x c p") 'flymake-goto-prev-error)

;; window navigation keys
(global-set-key "\C-ch" 'windmove-left)
(global-set-key "\C-c\C-h" 'windmove-left)
(global-set-key "\C-cj" 'windmove-down)
(global-set-key "\C-c\C-j" 'windmove-down)
(global-set-key "\C-ck" 'windmove-up)
(global-set-key "\C-c\C-k" 'windmove-up)
(global-set-key "\C-cl" 'windmove-right)
(global-set-key "\C-c\C-l" 'windmove-right)

;; Org
(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-dictionary "english")

(add-hook 'org-mode-hook 'flyspell-mode)
(add-hook 'org-mode-hook 'flyspell-buffer)

;; C++
(add-hook 'c++-mode-hook 'flymake-mode)
(defun my-c++-mode-hook ()
  (google-set-c-style)
  (google-make-newline-indent))

;; Java
(add-hook 'java-mode-hook
          (lambda ()
            (flymake-mode)
            (editorconfig-mode)))

;; Python
(setq python-shell-interpreter "/usr/local/bin/ipython"
     python-shell-interpreter-args "-i")
(add-hook 'python-mode-hook 'flymake-mode)

;; Themes to make me look beautiful
(load-theme 'ir-black t)
