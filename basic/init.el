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

(defun pdex-expand-file (file-base ext)
  (expand-file-name (concat "org-init/" file-base ext) user-emacs-directory))

(defun pdex-org-init (file-base)
  (let ((path-to-org (pdex-expand-file file-base ".org"))
        (path-to-elc (pdex-expand-file file-base ".elc")))
    (if (file-newer-than-file-p path-to-org path-to-elc)
      (org-babel-load-file path-to-org t)
      (load path-to-elc))))

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
