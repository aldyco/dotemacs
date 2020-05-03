;; load emacs 24's package system. Add MELPA repository.

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa-stable" . "https://stable.melpa.org/packages/") ; many packages won't show if using stable
   ;; '("melpa" . "https://melpa.milkbox.net/packages/")
   t)
  (add-to-list
   'package-archives
   ;; '("melpa-stable" . "https://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "https://melpa.milkbox.net/packages/")
   t))


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Hack" :foundry "unknown" :slant normal :weight normal :height 105 :width normal)))))

;; exec-path-from-shell
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; clj-refactor
(require 'clj-refactor)

(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(require 'xah-fly-keys)

(xah-fly-keys-set-layout "qwerty") ; required

;; possible layout values:

;; "azerty"
;; "azerty-be"
;; "colemak"
;; "colemak-mod-dh"
;; "dvorak"
;; "programer-dvorak"
;; "qwerty"
;; "qwerty-abnt"
;; "qwertz"
;; "workman"

(xah-fly-keys 1)
(define-key key-translation-map (kbd "ESC") (kbd "C-g"))
;; (defun my-xfk-command-color () (set-background-color "SlateBlue4"))
;; (defun my-xfk-insert-color () (set-background-color "SlateGray4"))

;; (add-hook 'xah-fly-command-mode-activate-hook 'my-xfk-command-color)
;; (add-hook 'xah-fly-insert-mode-activate-hook  'my-xfk-insert-color)

;; which-key
(require 'which-key)
(which-key-mode)



(global-subword-mode 1)			;camel case navigation
(global-set-key [remap goto-line] 'goto-line-preview)


;; smartparens
(require 'smartparens-config)
(smartparens-global-mode 1)



;;;;;;;;;;;;;;;;;;;;;;;;
;; keybinding management
(define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

(define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
(define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
(define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)

(define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)

(define-key smartparens-mode-map (kbd "C-M-n") 'sp-forward-hybrid-sexp)
(define-key smartparens-mode-map (kbd "C-M-p") 'sp-backward-hybrid-sexp)

(define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

(define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
(define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

(define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "M-<left>") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "M-<right>") 'sp-backward-barf-sexp)

(define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)
(define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
(define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
(define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

(define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key smartparens-mode-map (kbd "C-<left_bracket>") 'sp-select-previous-thing)
(define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)

(define-key smartparens-mode-map (kbd "M-F") 'sp-forward-symbol)
(define-key smartparens-mode-map (kbd "M-B") 'sp-backward-symbol)

(define-key smartparens-mode-map (kbd "C-\"") 'sp-change-inner)
(define-key smartparens-mode-map (kbd "M-i") 'sp-change-enclosing)

;; turn on highlight matching brackets when cursor is on one
(show-paren-mode 1)

;; (require 'helm-config)
;; (global-set-key (kbd "M-x") #'helm-M-x)
;; (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
;; (global-set-key (kbd "C-x C-f") #'helm-find-files)
;; (helm-mode 1)

;; (require 'ido-vertical-mode)
;; (ido-mode 1)
;; (ido-vertical-mode 1)
;; (setq ido-vertical-define-keys 'C-n-and-C-p-only)

(add-to-list 'load-path "~/.emacs.d/sublimity-master/")
(require 'sublimity)
(require 'sublimity-scroll)
;; (require 'sublimity-map) ;; experimental
(require 'sublimity-attractive)
(setq sublimity-attractive-centering-width 125)
(sublimity-mode 1)
(sublimity-attractive-hide-bars)
(sublimity-attractive-hide-vertical-border)
(sublimity-attractive-hide-fringes)

(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-center-content t)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; (require 'aggressive-indent)
;; (add-hook 'prog-mode-hook #'aggressive-indent-mode)

(require 'neotree)
(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (let ((project-dir (projectile-project-root))
	(file-name (buffer-file-name)))
    (neotree-toggle)
    (if project-dir
	(if (neo-global--window-exists-p)
	    (progn
	      (neotree-dir project-dir)
	      (neotree-find file-name)))
      (message "Could not find git project root."))))

(setq neo-autorefresh nil)

;; (global-set-key (kbd "<f8>") 'neotree-project-dir)
(define-key xah-fly-key-map (kbd "<f8>") 'neotree-project-dir)

(require 'doom-themes)

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
;; (load-theme 'doom-one t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Enable custom neotree theme (all-the-icons must be installed!)
(doom-themes-neotree-config)
;; or for treemacs users
(doom-themes-treemacs-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)


;; make backup to a designated dir, mirroring the full path

(defun my-backup-file-name (fpath)
  "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
  (let* (
        (backupRootDir "~/.emacs.d/backup/")
        (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path, for example, “C:”
        (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") ))
        )
    (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
    backupFilePath
  )
)

(setq make-backup-file-name-function 'my-backup-file-name)
(global-hl-line-mode 1)
(global-auto-revert-mode 1)


(setq clojure-align-forms-automatically t)

;; (smart-tabs-add-language-support clojure clojure-mode-hook
  ;; ((clojure-indent-line . clojure-basic-offset)
   ;; (clojure-indent-region . clojure-basic-offset)))
;; 
;; (smart-tabs-insinuate 'c 'javascript 'clojure)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (clojure . t)
   (emacs-lisp . t)))

(fset 'yes-or-no-p 'y-or-n-p)             ; y-or-n-p makes answering questions faster

;; ivy - swiper - counsel
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-use-selectable-prompt t)
(setq ivy-wrap t)
(setq ivy-extra-directories nil)
(setq enable-recursive-minibuffers t)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(define-key ivy-minibuffer-map (kbd "C-m") 'ivy-alt-done)

(all-the-icons-ivy-setup)
(setq all-the-icons-ivy-file-commands
      '(counsel-find-file counsel-file-jump counsel-recentf counsel-projectile-find-file counsel-projectile-find-dir))

(counsel-mode 1)
(defun my-bindkey-xfk-command-mode ()
  "Define keys for `xah-fly-command-mode-activate-hook'"
  (interactive)
  (define-key xah-fly-key-map (kbd "a")
    (if (fboundp 'counsel-M-x) 'counsel-M-x 'execute-extended-command))
  (define-key xah-fly-key-map (kbd "n")
    (if (fboundp 'swiper) 'swiper 'isearch-forward))
  (define-key xah-fly-key-map (kbd "SPC /") 'cljr-slash)
  (define-key xah-fly-key-map  (kbd "/") 'xah-goto-matching-bracket)
 
  ;; more here
  )

(add-hook 'xah-fly-command-mode-activate-hook 'my-bindkey-xfk-command-mode)



;; (require 'ivy-rich)
;; (ivy-rich-mode 1)
;; (setq ivy-format-function #'ivy-format-function-line)

;; (defun ivy-rich-switch-buffer-icon (candidate)
  ;; (with-current-buffer
      ;; (get-buffer candidate)
    ;; (let ((icon (all-the-icons-icon-for-mode major-mode)))
      ;; (if (symbolp icon)
	  ;; (all-the-icons-icon-for-mode 'fundamental-mode)
	;; icon))))

;; (setq ivy-rich--display-transformers-list
      ;; '(ivy-switch-buffer
        ;; (:columns
         ;; ((ivy-rich-switch-buffer-icon :width 2)
          ;; (ivy-rich-candidate (:width 30))
          ;; (ivy-rich-switch-buffer-size (:width 7))
          ;; (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
          ;; (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
          ;; (ivy-rich-switch-buffer-project (:width 15 :face success))
          ;; (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
         ;; :predicate
         ;; (lambda (cand) (get-buffer cand)))))


;; company   
(global-company-mode 1)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 3)   
;; (add-hook 'cider-repl-mode-hook #'company-mode)
;; (add-hook 'cider-mode-hook #'company-mode)
;; (add-hook 'clojure-mode-hook #'company-mode)

(require 'company-lsp)
(push 'company-lsp company-backends)

;; projectile
(require 'projectile)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-p") 'projectile-command-map)
(projectile-mode +1)
(setq projectile-completion-system 'ivy)
(setq projectile-project-search-path '("~/zenius/" "~/zenius/znet/bff/mobile"))


;; misc
(setq linum-format "%4d ")
(add-hook 'prog-mode-hook                 ; Show line numbers in programming modes
          (if (fboundp 'display-line-numbers-mode)
              #'display-line-numbers-mode
            #'linum-mode))

(beacon-mode 1)
(add-hook 'prog-mode-hook #'focus-mode)

(setq dimmer-fraction 0.5)


;; spaceline
(require 'spaceline-config)
(spaceline-spacemacs-theme)
(spaceline-all-the-icons-theme)




; working with logs
(require 'vlf-setup)


;; api blueprint
(autoload 'apib-mode "apib-mode"
        "Major mode for editing API Blueprint files" t)

(add-to-list 'auto-mode-alist '("\\.apib\\'" . apib-mode))


;; cider
(when (fboundp `cider-mode)
  (setq cider-print-fn nil))

(setq cider-eval-toplevel-inside-comment-form t)

(setq cider-invert-insert-eval-p t)
(setq cider-switch-to-repl-after-insert-p nil)

;; (require 'clj-refactor)



;; javascript & nodejs
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(require 'js2-refactor)
(require 'xref-js2)

(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js2-mode-map (kbd "C-k") #'js2r-kill)

;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
;; unbind it.
(define-key js-mode-map (kbd "M-.") nil)

(add-hook 'js2-mode-hook (lambda ()
			   (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

;; (require 'company-tern)

;; (add-to-list 'company-backends 'company-tern)
;; (add-hook 'js2-mode-hook (lambda ()
                           ;; (tern-mode)
                           ;; (company-mode)))
                           
;; Disable completion keybindings, as we use xref-js2 instead
;; (define-key tern-mode-keymap (kbd "M-.") nil)
;; (define-key tern-mode-keymap (kbd "M-,") nil)

;; Indium
(require 'indium)
(add-hook 'js-mode-hook #'indium-interaction-mode)
;; (setq indium-chrome-executable nil)


;; window-purpose
(require 'window-purpose)
(purpose-mode)

;; (add-to-list 'purpose-user-mode-purposes '(<major-mode> . <purpose>))
;; (add-to-list 'purpose-user-name-purposes '(<name> . <purpose>))
;; (add-to-list 'purpose-user-regexp-purposes '(<pattern> . <purpose>))
;; (setq purpose-use-default-configuration t) ; not really necessary, default is t
;; (purpose-compile-user-configuration) ; activates your changes


;; lsp-mode
;; (add-hook 'prog-mode-hook #'lsp)
(add-hook 'js2-mode-hook #'lsp-deferred)
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))



(defun my-company-transformer (candidates)
    (let ((completion-ignore-case t))
      (if (and (car candidates)
                   (get-text-property 0 'lsp-completion-prefix (car candidates)))
          (all-completions (company-grab-symbol) candidates)
        candidates
        )
      )
    )

(defun my-js-hook nil
  (make-local-variable 'company-transformers)
  (push 'my-company-transformer company-transformers))

(add-hook 'js2-mode-hook 'my-js-hook)


(setq lsp-auto-configure t)

;; dap-mode
(dap-mode 1)
(dap-ui-mode 1)
;; enables mouse hover support
(dap-tooltip-mode 1)
;; use tooltips for mouse hover
;; if it is not enabled `dap-mode' will use the minibuffer.
(tooltip-mode 1)

(require 'dap-node)
(require 'dap-chrome)

;; web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))

(setq web-mode-extra-auto-pairs
      '(("erb"  . (("beg" "end")))
        ("php"  . (("beg" "end")
                   ("beg" "end")))
	))

(add-hook 'web-mode-hook #'(lambda () (yas-activate-extra-mode 'html-mode)))

(setq web-mode-enable-auto-expanding t)

(setq web-mode-enable-css-colorization t)

(setq web-mode-enable-current-column-highlight t)
(setq web-mode-enable-current-element-highlight t)

(defun my-web-mode-hook ()
  (setq web-mode-enable-auto-pairing nil)
  (set (make-local-variable 'company-backends) '(company-css company-web-html company-yasnippet company-files)))

(yas-reload-all)

(add-hook 'web-mode-hook  'my-web-mode-hook )
(add-hook 'web-mode-hook  #'yas-minor-mode)

(defun sp-web-mode-is-code-context (id action context)
  (and (eq action 'insert)
       (not (or (get-text-property (point) 'part-side)
                (get-text-property (point) 'block-side)))))

(sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))

;; emmet-mode
(add-hook 'web-mode-hook  'emmet-mode)

(add-hook 'web-mode-before-auto-complete-hooks
    '(lambda ()
     (let ((web-mode-cur-language
  	    (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "php")
    	   (yas-activate-extra-mode 'php-mode)
      	 (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
    	   (setq emmet-use-css-transform t)
      	   (setq emmet-use-css-transform nil)))))


;; go-mode
(add-hook 'go-mode-hook 'lsp-deferred)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)





(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#1E2029" "#ff5555" "#50fa7b" "#f1fa8c" "#61bfff" "#ff79c6" "#8be9fd" "#f8f8f2"])
 '(counsel-mode t)
 '(custom-enabled-themes (quote (kaolin-galaxy)))
 '(custom-safe-themes
   (quote
    ("f7b0f2d0f37846ef75157f5c8c159e6d610c3efcc507cbddec789c02e165c121" "356e5cbe0874b444263f3e1f9fffd4ae4c82c1b07fe085ba26e2a6d332db34dd" "1c082c9b84449e54af757bcae23617d11f563fc9f33a832a8a2813c4d7dfb652" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "571a762840562ec5b31b6a9d4b45cfb1156ce52339e188a8b66749ed9b3b22a2" "0f1733ad53138ddd381267b4033bcb07f5e75cd7f22089c7e650f1bb28fc67f4" "3c915efd48b98c5e74393dd3e95bc89b69a9d0748479c5a99104d5611b1a12fd" "b13f76a2eb776abe9c17379d6d90f36cdac716678cd8b9138ba4b6c2a8fca378" "6e38567da69b5110c8e19564b7b2792add8e78a31dfb270168509e7ae0147a8d" "bee55ba5e878d0584db9b2fb33f75c348a3008fcfe8e05ab8cae897ca604fd95" "ef07cb337554ffebfccff8052827c4a9d55dc2d0bc7f08804470451385d41c5c" "f07729f5245b3c8b3c9bd1780cbe6f3028a9e1ed45cad7a15dd1a7323492b717" "9f08dacc5b23d5eaec9cccb6b3d342bd4fdb05faf144bdcd9c4b5859ac173538" "51043b04c31d7a62ae10466da95a37725638310a38c471cc2e9772891146ee52" "030346c2470ddfdaca479610c56a9c2aa3e93d5de3a9696f335fd46417d8d3e4" "886fe9a7e4f5194f1c9b1438955a9776ff849f9e2f2bbb4fa7ed8879cdca0631" "6b2636879127bf6124ce541b1b2824800afc49c6ccd65439d6eb987dbf200c36" "a3fa4abaf08cc169b61dea8f6df1bbe4123ec1d2afeb01c17e11fdc31fc66379" "d2e9c7e31e574bf38f4b0fb927aaff20c1e5f92f72001102758005e53d77b8c9" "f0dc4ddca147f3c7b1c7397141b888562a48d9888f1595d69572db73be99a024" "b46ee2c193e350d07529fcd50948ca54ad3b38446dcbd9b28d0378792db5c088" default)))
 '(dimmer-fraction 0.4 t)
 '(fci-rule-color "#6272a4")
 '(hl-sexp-background-color "#1c1f26")
 '(jdee-db-active-breakpoint-face-colors (cons "#1E2029" "#bd93f9"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1E2029" "#50fa7b"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1E2029" "#565761"))
 '(org-agenda-files (quote ("~/TODO")))
 '(package-selected-packages
   (quote
    (company-go go-mode clj-refactor emmet-mode yasnippet-snippets ivy-yasnippet react-snippets web-mode posframe company-lsp dap-mode lsp-ivy lsp-mode lsp-ui ivy-purpose window-purpose goto-line-preview exec-path-from-shell ag xref-js2 undo-tree company-ctags company-shell company-tern company-web tide indium dumb-jump git-timemachine git-gutter expand-region spaceline-all-the-icons spaceline popup-kill-ring dimmer beacon focus goto-last-point bm imenu-anywhere imenu-list gnu-elpa-keyring-update calfw-org cider-eval-sexp-fu counsel-projectile counsel-tramp graphql apib-mode avy multi-term logview vlf json-mode kaolin-themes flx smex all-the-icons-ivy ivy-rich zenburn-theme use-package material-theme magit doom-modeline company aggressive-indent ac-cider android-mode smart-tabs-mode doom-themes smartparens clojure-snippets rainbow-delimiters dashboard projectile page-break-lines neotree dracula-theme which-key xah-fly-keys)))
 '(vc-annotate-background "#282a36")
 '(vc-annotate-color-map
   (list
    (cons 20 "#50fa7b")
    (cons 40 "#85fa80")
    (cons 60 "#bbf986")
    (cons 80 "#f1fa8c")
    (cons 100 "#f5e381")
    (cons 120 "#face76")
    (cons 140 "#ffb86c")
    (cons 160 "#ffa38a")
    (cons 180 "#ff8ea8")
    (cons 200 "#ff79c6")
    (cons 220 "#ff6da0")
    (cons 240 "#ff617a")
    (cons 260 "#ff5555")
    (cons 280 "#d45558")
    (cons 300 "#aa565a")
    (cons 320 "#80565d")
    (cons 340 "#6272a4")
    (cons 360 "#6272a4")))
 '(vc-annotate-very-old-color nil))

