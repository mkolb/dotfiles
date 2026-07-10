; -*-Lisp-*-
;; Author: Matt Kolb <kolb722@gmail.com>
;; Used with: Emacs 30.2 on Ubuntu Linux/WSL2

;; <packages>
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))
;; </packages>

;; <programming>
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(use-package company :ensure t)
(use-package flycheck :ensure t)

;; <typescript>
(use-package prettier-js
    :ensure t
    :hook (typescript-mode . prettier-js-mode))
(add-hook 'typescript-mode-hook 'eglot-ensure)
;; </typescript>

;; <eglot>
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((js-mode typescript-mode)
		 . ("tsc" "--lsp" "--stdio"))))
;; </eglot>
;; </programming>

;; <text>
(add-hook 'text-mode-hook
	  'flyspell-mode)
;; </text>

;; <org>
(setq org-log-done 'time)
;; </org>

;; <appearance>
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(set-face-attribute 'default nil :font "Intel One Mono-16" )
(set-frame-font "Intel One Mono-16" nil t)



;; <dracula>
;; Don't change the font size for some headings and titles (default t)
(setq dracula-enlarge-headings nil)

;; Adjust font size of titles level 1 (default 1.3)
(setq dracula-height-title-1 1.25)

;; Adjust font size of titles level 2 (default 1.1)
(setq dracula-height-title-2 1.15)

;; Adjust font size of titles level 3 (default 1.0)
(setq dracula-height-title-3 1.05)

;; Adjust font size of document titles (default 1.44)
(setq dracula-height-doc-title 1.4)

;; Use less pink and bold on the mode-line and minibuffer (default nil)
(setq dracula-alternate-mode-line-and-minibuffer t)

(load-theme 'dracula t)
;; </dracula>


;; </appearance>

;; <eshell>
(setq eshell-cmpl-cycle-completions nil)
;(add-to-list 'exec-path "/home/mak/node/bin/")
;; </eshell>

(use-package exec-path-from-shell
  :config (exec-path-from-shell-initialize))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company dracula-theme exec-path-from-shell flycheck go-mode lsp-ui
	     magit prettier-js rainbow-delimiters typescript-mode
	     web-mode yaml-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
