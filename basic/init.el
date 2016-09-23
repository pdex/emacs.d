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
  (message "use-package not installed")
  (package-install 'use-package))

;; Use use-package now
(use-package org
  :ensure t
  :pin org)

(defun pdex-org-init (file-base)
  (org-babel-load-file (expand-file-name (concat "org-init/" file-base ".org") user-emacs-directory)))

(pdex-org-init "editorconfig")
(pdex-org-init "emacs-cheatsheet")
(pdex-org-init "shutdown")
(pdex-org-init "window")

(let ((custom-file (expand-file-name "custom.el" user-emacs-directory)))
  (if (file-exists-p custom-file)
    (load custom-file)))
