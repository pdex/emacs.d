;; Setup package.el
(require 'package)
;; since we call package-initialize, we don't need emacs startup.el to do the same
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
  '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
  '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives 
  '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Use use-package now
(use-package org
  :ensure t
  :pin org)

(org-babel-load-file (expand-file-name "org-init/emacs-cheatsheet.org" user-emacs-directory))
(org-babel-load-file (expand-file-name "org-init/shutdown.org" user-emacs-directory))

(let ((custom-file (expand-file-name "custom.el" user-emacs-directory)))
  (if (file-exists-p custom-file)
    (load custom-file)))
