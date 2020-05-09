;;; init.el --- My Emacs configuration

;;; Commentary:

;; Following lines load an Org file and build the configuration code out of it.

;;; Code:

(let ((gc-cons-threshold most-positive-fixnum))

  ;; Set repositories
  (require 'package)
  (setq-default
   load-prefer-newer t
   package-enable-at-startup nil)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
  (package-initialize)

  ;; Set explicit priorities (gnu is the default package-archive)
  (setq package-archive-priorities '(("org" . 4)
                   ("melpa" . 3)
                   ("melpa-stable" . 2)
                   ("gnu" . 1)))

  ;; Install dependencies
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package t))
  (setq-default
   use-package-always-defer t
   use-package-always-ensure t)

  (use-package use-package-ensure-system-package
  :ensure t)

  ;; Use latest Org
  (use-package org :ensure org-plus-contrib)

  ;; disable emacs asking following git symbolink
  (setq vc-follow-symlinks nil)


  ;; Tangle configuration
  (org-babel-load-file (expand-file-name "dotemacs.org" user-emacs-directory))
  (garbage-collect))

;;; init.el ends here
